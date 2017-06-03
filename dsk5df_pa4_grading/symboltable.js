/**
 * A simple JavaScript-object-based hashtable implementation that maintains
 * a history of items. If you insert a new value, the old value is hidden,
 * rather than destroyed; removing the item restores the old value.
 *
 * Functionality is similar to (and more limited than):
 * https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html
 *
 * Usage:
 * you can create a new symboltable as follows:
 *
 * var foo = new SymbolTable()
 *
 * foo is now a symbol table that you can pass around. It has 'add', 'clear',
 * 'find', 'mem', and 'remove' methods.
 *
 * You may modify this file as you see fit.
 *
 * @author Kevin Angstadt angstadt@virginia.edu
 */

exports.SymbolTable = function() {
    // Our private symbol table object
    var table = {};

    // Now, we define several public methods

    /*
     * Empty a hash table.
     */
    this.clear = function() {
        table = {};
    };

    /*
     * add( x, y ) adds a binding of x to y in table tbl. Previous bindings for
     * x are not removed, but simply hidden.
     */
    this.add = function(x, y) {
        if(Object.keys(table).indexOf(x) > -1) {
            // push onto stack
            table[x].push(y);
        } else {
            // create a new stack
            table[x] = [y];
        }
    };

    /*
     * find( x ) returns the current binding of x in tbl, or throws "not found" if
     * no such binding exists.
     */
    this.find = function(x) {
        if(Object.keys(table).indexOf(x) > -1 && (table[x].length > 0) ) {
            return table[x][table[x].length-1];
        } else {
            // throw "not found";
            // console.log(x, "not found in table!!");
            return "I didn't find anything master";
        }
    };

    /*
     * Returns whether x is bound in the table.
     */
    this.mem = function(x) {
        return x in table;
    };

    /*
     * removes the current bidning of x, restoring the previous binding if it
     * exists.
     */
    this.remove = function(x) {
        if(Object.keys(table).indexOf(x) > -1) {
            table[x].pop();
        }
    };

	this.print = function(x){
		return table;
	}
}
