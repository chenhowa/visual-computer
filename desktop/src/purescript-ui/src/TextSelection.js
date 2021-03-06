function doLog() {
    return false;
}

function log(val) {
    if(doLog()) {
        console.log(val)
    }
}


function countTextChildren(node, endNode, count) {
    if(node === endNode) {
        log("EQUAL")
        log(node)
        return {
            count: 0,
            done: true
        }

        //return -1
        //return count
    }

    if(node.nodeType === 3) {
        return {
            count: node.length,
            done: false
        }
    }

    var countTrack = count
    var results = {}
    if(node.childNodes) {
        for(var i = 0; i < node.childNodes.length; i++) {
            results = countTextChildren(node.childNodes[i], endNode, 0)

            log('added: ' + results.count)
            countTrack += results.count

            if (results.done) {
                break;
            }
        }
    }

    return {
        count: countTrack,
        done: results.done 
    }
}

function getRoot(rootClass, node) {
    if (isRootClass(node, rootClass)) {
        return node
    } else if (node.parentNode) {
        return getRoot(rootClass, node.parentNode)
    } else {
        throw new Error("getRoot failed")
    }
}

function isRootClass(node, rootClass) {
    return node.nodeType !== 3 && node.classList && node.classList.contains(rootClass)
}

/*Returns index of the starting character in the selection*/
exports.startIndex = function startIndex(rootClass) {
    var selection = window.getSelection()
    var anchorNode = selection.anchorNode
    var anchorOffset = selection.anchorOffset

    var rootNode = getRoot(rootClass, anchorNode)
    /*log("root was: ")
    log(rootNode)
    log("anchor was: ")
    log(anchorNode)*/
    var count = countTextChildren(rootNode, anchorNode, 0).count

    var start = count + anchorOffset - 1
    log("start index found was " + start)

    return Math.max(start, 0)
}

/*Returns index of the ending character in the selection*/
exports.endIndex = function(rootClass) {
    var selection = window.getSelection()
    var focusNode = selection.focusNode
    var focusOffset = selection.focusOffset

    var rootNode = getRoot(rootClass, focusNode)
    var count = countTextChildren(rootNode, focusNode, 0).count

    var end = count + focusOffset - 1
    log("end index found was " + end)

    return end
}

/*Returns a string containing the text of the entire selection*/
exports.selection = function(unit) {
    var selection = window.getSelection()
    return selection.toString()
}

exports.setSelection = function(rootClass, startIndex, endIndex) {
    log("selection start: " + startIndex)
    log("selection end: " + endIndex)
    var rootNodes = document.getElementsByClassName(rootClass)
    if (rootNodes.length <= 0) {
        throw new Error("setSelection: rootClass " + rootClass + " not found")
    }
    var rootNode = rootNodes[0]

    var selection = window.getSelection()
    selection.removeAllRanges()
    var range = document.createRange()

    var start = getNodeAndOffset(rootNode, rootNode, startIndex, -1)
    if(start.done) {
        range.setStart(start.node, start.offset)
    } else {
        range.setStart(start.node, Math.abs(startIndex - start.offset))
    }


    var end = getNodeAndOffset(rootNode, rootNode, endIndex, -1)
    range.setEnd(end.node, end.offset)
    if(end.done) {
        range.setEnd(end.node, end.offset)
    } else {
        range.setEnd(end.node, Math.abs(startIndex - end.offset))
    }

    selection.addRange(range)

    return 5
    
    function getNodeAndOffset(currentNode, previousNode, endIndex, currentIndex) {
        if(currentIndex === endIndex) {
            return logReturn({
                node: previousNode,
                offset: Math.abs(currentIndex - endIndex),
                done: true
            })
        }

        if(currentNode.nodeType === 3) {
            if(currentNode.length >= Math.abs(currentIndex - endIndex)) {
                return logReturn({
                    offset: Math.abs(currentIndex - endIndex),
                    node: currentNode,
                    done: true
                })
            } else {
                return logReturn({
                    offset: currentNode.length,
                    node: currentNode,
                    done: false
                })
            }
        }

        var indexTrack = currentIndex
        var newPrevious = currentNode
        var textCount = 0
        for(var i = 0; i < currentNode.childNodes.length; i++) {
            var result = getNodeAndOffset(currentNode.childNodes[i], newPrevious, endIndex, indexTrack);
            if(result.done) {
                return logReturn (result);
            } else {
                newPrevious = result.node,
                indexTrack += result.offset
                textCount += result.offset
            }
        }

        //log("composite index of " + indexTrack)
        //log("text count for this child of " + textCount)


        return logReturn ({
            node: newPrevious,
            offset: textCount,
            done: false
        })

        function logReturn(thing) {
            log(thing)
            return thing
        }
    }
}