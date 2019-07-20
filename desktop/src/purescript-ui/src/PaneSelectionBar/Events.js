
var electron = require('electron')


exports.onContextImpl = function(node, fn) {
    var menu = new electron.remote.Menu()
    menu.append(new electron.remote.MenuItem({ 
        label: 'Program', 
        click: function(item, window, event) { 
            fn("textEditor")()
        } 
    }))
    menu.append(new electron.remote.MenuItem({ type: 'separator' }))
    menu.append(new electron.remote.MenuItem({
        click: function(item, window, event) {
            fn("memoryEditor")()
        },
        label: 'Memory', 
        type: 'checkbox', 
        checked: true }))


    return function() {
        return node.addEventListener('contextmenu', function(event) {
            event.preventDefault()
            menu.popup({ window: electron.remote.getCurrentWindow() })
        }, false)
    }
}