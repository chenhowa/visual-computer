
var electron = require('electron')
exports.menu = new electron.remote.Menu();
exports.menuItemOptions = [];

exports.initializeMenu = function(showTextEditor, showMemoryEditor) {
    exports.menuItemOptions.push({
        id: '1',
        label: 'Program', 
        type: 'checkbox',
        checked: showTextEditor,
    })
    exports.menuItemOptions.push({
        id: '2',
        type: 'separator' 
    })
    exports.menuItemOptions.push({
        id: '3',
        label: 'Memory', 
        type: 'checkbox', 
        checked: showMemoryEditor,
    })

    return 5;
}



exports.onContextImpl = function(node, fn) {
    exports.menuItemOptions[0].click = function(item, window, event) { 
        console.log("CLICKED PROGRAM")
        fn("textEditor")()
    }

    exports.menuItemOptions[2].click = function(item, window, event) {
        console.log("CLICKED MEMORY")
        fn("memoryEditor")()
    }

    for(var i = 0; i < exports.menuItemOptions.length; i++) {
        exports.menu.append(new electron.remote.MenuItem(exports.menuItemOptions[i]))
    }

    return function() {
        return node.addEventListener('contextmenu', function(event) {
            event.preventDefault()
            exports.menu.popup({ window: electron.remote.getCurrentWindow() })
        }, false)
    }
}