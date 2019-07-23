/*window.$ = require('jquery')
window.jQuery = $
window.$ = require('jquery-ui')*/

//var Split = require('split.js')


exports.setResizable = function() {
    setTimeout(function() {
        $( ".cl-column" ).resizable();
    }, 100)
    
    

    /*Split(["#one", "#two"], {
        sizes: [25, 75],
    })*/
}