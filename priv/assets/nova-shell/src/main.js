'use strict';

const Bert = require('bert-elixir');
const socket = new WebSocket("ws://" + location.host + base_url + "/term");
socket.binaryType = "arraybuffer";

var welcome_msg = 'Welcome to Nova Shell. Here you can run Erlang commands.\r\nIf there\'s any question please check http://www.erlang.org';


var term = new window.Terminal({
    cursorBlink: true,
    cols: 160,
    rows: 40
});

term.open(document.getElementById('terminal'));



function init() {
    if (term._initialized) {
        return;
    }

    term._initialized = true;
    term.command = '';

    term.prompt = () => {
        term.write('\r\n$ ');
    };
    term.write(welcome_msg);
    prompt(term);

    term.onData(e => {
        switch (e) {
        case '\u0003': // Ctrl+C
            term.write('^C');
            prompt(term);
            break;
        case '\r': // Enter
            runCommand(term, term.command);
            term.write("\n");
            var inputLengh = term.command.length + 2;
            for (var i = 0; i < inputLengh; i++) {
                term.write('\b');
            }
            term.command = '';
            break;
        case '\u007F': // Backspace (DEL)
            // Do not delete the prompt
            if (term._core.buffer.x > 2) {
                term.write('\b \b');
                if (term.command.length > 0) {
                    term.command = term.command.substr(0, term.command.length - 1);
                }
            }
            break;
        case '\u0009':
            socket.send("!!_comp_!!" + term.command);
            break;
        default:
            if (e >= String.fromCharCode(0x20) && e <= String.fromCharCode(0x7E) || e >= '\u00a0') {
                term.command += e;
                term.write(e);
            }
        }
    });
}

function prompt(term) {
    term.command = '';
    term.write('\r\n$ ');
}


function decode(data) {
    // Create a "byte array" of the data that we can parse.
    let dview = new DataView(data),
        bytes = new Uint8Array(dview.byteLength)
    let result = [];
    for (var i = dview.byteOffset; i < bytes.length; i++) {
        bytes[i] = dview.getUint8(i);
        result[i] = parseInt(bytes[i].toString());
    }
    result = result.map(x => String.fromCharCode(x)).join('')

    // Use Bert to decode the byte array.
    return Bert.decode(result)
}

socket.onmessage = (event) => {
    let json = decode(event.data);
    switch(json.action) {
    case "completion_list":
        json.items.forEach(element => {
            term.write(element + '\r\n');
        });
        term.write('$ ' + term.command);
        break;
    case "completion_not_found":

        break;
    case "completion":
        term.command += json.completion;
        term.write(json.completion);
        break;
    case "command_success":
        term.write("> " + json.result);
        term.prompt();
        break;
    case "command_failed":
        term.write("> " + json.reason);
        term.prompt();
        break;
    }
}


function runCommand(term, command) {
    if (command.length > 0) {
        console.log("Sending command ", command);
        socket.send(Bert.encode(command));
        return;
    }
}

init();
