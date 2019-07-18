var rxjs = require('rxjs')
var operators = require('rxjs/operators')

exports.onTripleClickImpl = function(node, fn) {
    return function() {
        var clicks = rxjs.fromEvent(node, 'click')
        return clicks
            .pipe(operators.bufferTime(500, rxjs.asyncScheduler))
            .pipe(operators.filter(function(clicks){
                return clicks.length > 1
            }))
            .subscribe(function(event) {
                console.log('triple click event was working!');
                console.log(event)
                fn(event)
            })
    }
    
}

exports.isUndefined = function(val) {
    console.log('val was ' + val)
    return val === undefined
}

exports.maybeTarget = function(query) {
    var temp = undefined
    if(document) {
        temp = document.getElementById(query)
        if(temp) {
            return temp
        }

        temp = document.getElementsByClassName(query)
        if(temp.length > 0) {
            return temp[0]
        }

        temp = document.querySelector(query);
        if(temp) {
            return temp
        }
    }

    return undefined
}

exports.unsafeGetDefined = function(val) {
    if(val !== undefined) {
        return val
    } else {
        throw new Error("tried to get defined value from undefined")
    }
}