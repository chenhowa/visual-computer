var electron = require('electron')

function openFileDialog() {
    var options = {
        title: "ASM"
    }

    var strings = electron.remote.dialog.showOpenDialog(null, options, null)
    if (strings) {
        return strings[0]
    } else {
        return "HEY"
    }
}

exports.openFileDialog = openFileDialog