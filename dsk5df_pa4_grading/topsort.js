//Sets up topological sort
module.exports = {
    sortTopo: function(graph) {
        var x = {};
        // dictionary of nodes -> incoming nodes
        var y = {};
        // list of starting nodes (no incoming edges)
        var start_nodes = [];

        // loop through edges and build start_nodes
        for (var i = 0; i < graph.length; i++) {
            var start = graph[i][0];
            var end = graph[i][1];

            // remove edges that do not belong in start_nodes
            if (!checkin(start, start_nodes)) {
                start_nodes.push(start);
            }

            if (!checkin(end, start_nodes)) {
                var ind = start_nodes.indexOf(end);
                if (ind != -1) {
                    start_nodes.splice(ind, 1);
                }
            }

            //  map edges to their incoming edges
            if (!(end in y)) {
                y[end] = [start];
            } else {
                var temp_list = y[end];
                temp_list.push(start);
                y[end] = temp_list;
            }

            //  map edges to their outgoing edges
            if (!(start in x)) {
                x[start] = [end];
            } else {
                var temp_list = x[start];
                temp_list.push(end);
                x[start] = temp_list;
            }
        }

        //create list of nodes with no incoming edges edges
        for (var key in y) {
            if (y.hasOwnProperty(key)) {
                start_nodes.forEach(function(snode) {
                    if (snode == key) {
                        var i = start_nodes.indexOf(snode);
                        if (i != -1) {
                            start_nodes.splice(i, 1);
                        }
                    }
                });
            }
        }

        //call topologicalSort
        stack = [];
        return topologicalSort(start_nodes, stack, x, y);
    }
};


//topological sort
var topologicalSort = function(S, L, x, y) {

    //loop through the queue until it is empty
    var cycle = false;
    if (S.length <= 0) {
        cycle = true;
    }
    while (S.length > 0) {
        S.sort();
        var node = S[0]

        //remove it from the queue and add it to the results
        var index = S.indexOf(node);
        if (index != -1) {
            S.splice(index, 1);
        }
        L.splice(0, 0, node)

        //if the node is an outgoing edge
        if (node in x) {
            x[node].forEach(function(m) {
                //remove the node of the incoming edge
                var index = y[m].indexOf(node);
                if (index != -1) {
                    y[m].splice(index, 1);
                }
                //if the current outgoing edge no longer has incoming edges,
                //add it to the queue
                if (y[m].length == 0) {
                    delete y[m];
                    S.push(m);
                }
            });
            delete x[node];
        }
    }

    //if there are any edges in the incoming edge graph, there must be a cycle
    size = Object.keys(y).length;
    if (size > 0 || cycle) {
        return "cycle";
    } else {
        L.reverse();
        return L;
    }
}

//check if an item is in a list
var checkin = function(item, list) {
    for (var index = 0; index < list.length; index++) {
        if (item == list[index]) {
            return true;
        }
    }
    return false;
}
