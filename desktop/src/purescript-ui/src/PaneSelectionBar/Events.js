
var electron = require('electron')
exports.menu = new electron.remote.Menu();
exports.menuItemOptions = [];

exports.initializeMenu = function(showTextEditor, showMemoryEditor) {
    exports.menuItemOptions.push({
        label: 'Layouts',
        submenu: [
            { label: "Tab"
            },
            { label: "Column"
            }
        ]
    })
    exports.menuItemOptions.push({
        label: 'Program', 
        type: 'checkbox',
        checked: showTextEditor,
    })
    exports.menuItemOptions.push({
        type: 'separator' 
    })
    exports.menuItemOptions.push({
        label: 'Memory', 
        type: 'checkbox', 
        checked: showMemoryEditor,
    })

    return 5;
}



exports.onContextImpl = function(node, fn) {
    exports.menuItemOptions[0].submenu[0].click = function(item, window, event) {
        console.log("CLICKED TAB")
        fn("tabLayout")()
    }

    exports.menuItemOptions[0].submenu[1].click = function(item, window, event) {
        console.log("CLICKED COLUMN")
        fn("columnLayout")()
    }

    exports.menuItemOptions[1].click = function(item, window, event) { 
        console.log("CLICKED PROGRAM")
        fn("textEditor")()
    }

    exports.menuItemOptions[3].click = function(item, window, event) {
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