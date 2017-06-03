//imports
fs = require('fs');
topsort = require("./topsort.js");
symboltable = require("./symboltable.js");


//-----------SECTION0: define the built in functions of IO, Object, and String----------------------------

// Base functions that belong to IO
var out_string = new Method(new Id(0, "out_string"), [new Formal(new Id(0, "x"), new Id(0, "String"))], new Id(0, "SELF_TYPE"), new Exp(0, new Id(0, "self")), "IO");
var out_int = new Method(new Id(0, "out_int"), [new Formal(new Id(0, "x"), new Id(0, "Int"))], new Id(0, "SELF_TYPE"), new Exp(0, new Id(0, "self")), "IO");
var in_string = new Method(new Id(0, "in_string"), [], new Id(0, "String"), new Exp(0, new String("hello")), "IO");
var in_int = new Method(new Id(0, "in_int"), [], new Id(0, "Int"), new Exp(0, new Integer(555)), "IO");

//Base functions that belong to Object
var methabort = new Method(new Id(0, "abort"), [], new Id(0, "Object"), new Exp(0, new Integer(555)), "Object");
var type_name = new Method(new Id(0, "type_name"), [], new Id(0, "String"), new Exp(0, new String("555")), "Object");
var copy = new Method(new Id(0, "copy"), [], new Id(0, "SELF_TYPE"), new Exp(0, new Id(0, "self")), "Object");

//Base functions that belong to String
var melen = new Method(new Id(0, "length"), [], new Id(0, "Int"), new Exp(0, new Integer(555)), "String");
var meconcat = new Method(new Id(0, "concat"), [new Formal(new Id(0, "s"), new Id(0, "String"))], new Id(0, "String"), new Exp(0, new String("hello")), "String");
var mesubs = new Method(new Id(0, "substr"), [new Formal(new Id(0, "i"), new Id(0, "Int")), new Formal(new Id(0, "l"), new Id(0, "Int"))], new Id(0, "String"), new Exp(0, new String("hello")), "String");



//------------SECTION 1: setup objects for storing expression info--------------------------------------

function CoolClass(cname, inhert, features) {
    this.cname = cname;
    this.inherit = inhert;
    this.features = features;
    this.attrib = [];
    this.method = [];
}

function Method(mname, formals, mtype, mbody, mdef) {
    this.definition = mdef;
    this.mname = mname;
    this.formals = formals;
    this.mtype = mtype;
    this.mbody = mbody;
    this.fmeth = "Method";
    this.rettype = "";
}

function Attribute(fname, ftype, initals) {
    this.fname = fname;
    this.ftype = ftype;
    this.finit = initals;
    this.fmeth = "Attribute";
    this.rettype = "";
}

function Formal(fname, ftype) {
    this.fname = fname;
    this.ftype = ftype;

}

function Exp(eloc, ekind) {
    this.eloc = eloc;
    this.ekind = ekind;
}

function Id(loc, name) {
    this.etype = "identifier";
    this.loc = loc;
    this.name = name;
    this.rettype = "";
}

function Integer(ival) {
    this.etype = "integer";
    this.value = ival;
    this.rettype = "Int";
}

function Bool(ival) {
    this.etype = "bool";
    this.value = ival;
    this.rettype = "Bool";
}

function String(ival) {
    this.etype = "string";
    this.value = ival;
    this.rettype = "String";
}

function Not(ival) {
    this.etype = "not";
    this.value = ival;
    this.rettype = "";
}

function Comp(mtype, item1, item2) {
    this.etype = mtype;
    this.val1 = item1;
    this.val2 = item2;
    this.rettype = "";
}

function Negate(item) {
    this.etype = "negate";
    this.value = item;
    this.rettype = "";
}

function IsVoid(etype, line) {
    this.etype = etype;
    this.value = line;
    this.rettype = "";
}

function NewType(etype, item) {
    this.etype = etype;
    this.value = item;
    this.rettype = "";
}

function Block(etype, evalue) {
    this.etype = etype;
    this.value = evalue;
    this.rettype = "";
}

function Assign(etype, eid, exp) {
    this.etype = etype;
    this.eid = eid;
    this.exp = exp;
    this.rettype = "";
}

function SDispatch(etype, eid, argsa) {
    this.etype = etype;
    this.eid = eid;
    this.args = argsa;
    this.rettype = "";
}

function DDispatch(ekind, eexp, etype, eid, args) {
    this.etype = ekind;
    this.exp = eexp;
    this.dtype = etype;
    this.did = eid;
    this.arglist = args;
    this.rettype = "";
}

function If(etype, econd, itrue, ifalse) {
    this.etype = etype;
    this.cond = econd;
    this.itrue = itrue;
    this.ifalse = ifalse;
    this.rettype = "";
}

function While(etype, econd, ival) {
    this.etype = etype;
    this.cond = econd;
    this.value = ival;
    this.rettype = "";
}

function Action(myid, mytype, myexp) {
    this.id = myid;
    this.atype = mytype;
    this.exp = myexp;
    this.rettype = "";
}

function Case(myexp, actlist) {
    this.etype = "case";
    this.exp = myexp;
    this.action = actlist;
    this.rettype = "";
}

function Let(letlist, inexp) {
    this.etype = "let";
    this.letlist = letlist;
    this.inexp = inexp;
    this.rettype = "";
}

//------------SECTION 2: Read File and build AST-------------------------------

function readFile() {
    var contents = fs.readFileSync(process.argv[2]).toString();
    contents = contents.split("\n");
    contents.pop();
    return contents;
}

//load ast
var myAST = readFile();
var myindex = 0;
var userClasses = [];
var identifiers = [];
var fname = process.argv[2].slice(0, -4);
fname += "-type";



//checks for missing main function
if (myAST.indexOf("main") == -1) {
    console.log("ERROR: 0: Type-Check: no main method in entire AST");
    process.exit();
}


//helper function to consume input from the ast
function read(ast) {
    var item = myAST[myindex];
    myindex++;
    return item;
}

//------------SECTION 3: Traverse AST and Build Expressions (in Object form)-------------------------------

//read a list with a function
function read_list(worker, clname) {
    var llength = read();
    var items = [];
    for (var i = 0; i < llength; i++) {
        items.push(worker(clname));
    }
    return items;
}

//read cool program
function readCoolProgram() {
    read_list(read_cool_class);
}

//returns a cool class
function read_cool_class() {
    var cname = read_id();
    var inherit = "";
    var citem = read();
    if (citem == "inherits") {
        inherit = read_id();
    }
    //don't read exp of no inherit
    // else if(citem == "no_inherits"){
    // }
    // else{
    // 	console.log("nope!", citem);
    // }


    //read methods and attributes for the inherited class
    var features = read_list(read_features, cname.name);

    //add class to the possible user classes
    userClasses.push(new CoolClass(cname, inherit, features));

    //return the current class
    return new CoolClass(cname, inherit, features);
}

//reads features function
function read_features(clname) {
    var citem = read();

    //creates and returns an attribute or method
    if (citem == "attribute_no_init") {
        var fname = read_id();
        var ftype = read_id();

        return new Attribute(fname, ftype, []);
    } else if (citem == "attribute_init") {
        var fname = read_id();
        var ftype = read_id();
        var finit = read_exp();

        return new Attribute(fname, ftype, finit);
    } else if (citem == "method") { // method
        var mname = read_id();
        var formals = read_list(read_formal);
        var mtype = read_id();
        var mbody = read_exp();

        return new Method(mname, formals, mtype, mbody, clname);
    }
}

//returns a Formal
function read_formal() {
    var fname = read_id();
    var ftype = read_id();

    //if the paramter uses the keyword self or tries to have type self, throw error
    if (fname.name == "self" || ftype.name == "self") {
        console.log("ERROR: " + fname.loc + ": Type-Check: cannot use self as a parameter!!");
        process.exit();
    }

    //if the paramter uses the keyword SELF_TYPE or tries to have type SELF_TYPE, throw error
    if (fname.name == "SELF_TYPE" || ftype.name == "SELF_TYPE") {
        console.log("ERROR: " + fname.loc + ": Type-Check: cannot use SELF_TYPE as a parameter!!");
        process.exit();
    }

    //otherwise create and return self type
    return new Formal(fname, ftype);
}

//returns an ID
function read_id() {
    var loc = read();
    var name = read();
    return new Id(loc, name);
}

//reads every type of expression and returns it
function read_exp() {

    //location, expression information, expression type
    var eloc = read();
    var ekind = "";

    var citem = read();

    //sets expression information
    if (citem == "integer") {
        var ival = read();
        ekind = new Integer(ival);
    } else if (citem == "not") {
        ekind = new Not(read_exp());
    } else if (check(citem, ["true", "false"])) {
        ekind = new Bool(citem);
    } else if (citem == "string") {
        var ival = read();
        ekind = new String(ival);
    } else if (citem == "identifier") {
        ekind = read_id();
    }
    //all arithmetic and comparisons
    else if (check(citem, ["lt", "eq", "le", "plus", "minus", "times", "divide"])) {
        var item1 = read_exp();
        var item2 = read_exp();
        ekind = new Comp(citem, item1, item2);
    } else if (citem == "negate") {
        ekind = new Negate(read_exp());
    } else if (citem == "isvoid") {
        var item = read_exp();
        ekind = new IsVoid(citem, item);
    } else if (citem == "new") {
        ekind = new NewType(citem, read_id());
    } else if (citem == "block") {
        var blist = [];
        var len = read();

        //push list of expressions
        for (var i = 0; i < len; i++) {
            blist.push(read_exp());
        }
        ekind = new Block("block", blist);
    } else if (citem == "assign") {
        ekind = new Assign("assign", read_id(), read_exp());
    } else if (citem == "self_dispatch") {
        var sdlist = [];
        var myid = read_id();
        var len = read();
        //push arguments
        for (var i = 0; i < len; i++) {
            var item = read_exp();
            sdlist.push(item);
        }
        ekind = new SDispatch("self_dispatch", myid, sdlist);
    } else if (citem == "if") {
        ekind = new If("if", read_exp(), read_exp(), read_exp());
    } else if (citem == "while") {
        ekind = new While("while", read_exp(), read_exp());
    } else if (citem == "case") {
        actlist = [];
        var cexp = read_exp();
        var len = read();

        //read case list
        for (var i = 0; i < len; i++) {
            var myid = read_id();
            var mytype = read_id();
            var myexp = read_exp();

            //throw and error of a case is of type self
            if (myid.name == "self") {
                console.log("ERROR: " + myid.loc + ": Type-Check: cannot use self as a thing in !!");
                process.exit();
            }

            //add to list of possible actions
            actlist.push(new Action(myid, mytype, myexp));
        }

        ekind = new Case(cexp, actlist);
    } else if (citem == "let") {
        var letlist = [];
        var len = read();
        var myid;
        var mytype;
        var myexp;

        //read let binding list
        for (var i = 0; i < len; i++) {
            var lettype = read();
            myid = read_id();
            mytype = read_id();
            if (lettype == "let_binding_init") {
                myexp = read_exp();
            } else if (lettype == "let_binding_no_init") {
                myexp = "";
            }
						//TODO: check for SELF_TYPE
						//throw error if self is used in let
            if (myid.name == "self") {
                console.log("ERROR: " + myid.loc + ": Type-Check: cannot use self as a thing in !!");
                process.exit();
            }
            letlist.push(new Action(myid, mytype, myexp));
        }
        var inexp = read_exp();
        ekind = new Let(letlist, inexp);
    }
    //check for dynamic/static dispatch
    else if (check(citem, ["dynamic_dispatch", "static_dispatch"])) {
        var mexp = read_exp();
        var mtype = "";
        if (citem == "static_dispatch") {
            mtype = read_id();
        }
        var mid = read_id();
        var len = read();
        var arglist = [];
        for (var i = 0; i < len; i++) {
            arglist.push(read_exp());
        }
        ekind = new DDispatch(citem, mexp, mtype, mid, arglist);
    } else {
        console.log("Have not done:" + citem + " " + process.argv[2]);
    }
    return new Exp(eloc, ekind);
}

//read cool program recursively
readCoolProgram();

//list of user classes(to be filled) and baseclasses
var base_classes = ["Int", "String", "Bool", "IO", "Object"];
var user_classes = [];

//throw error if base classes are redefined
for (var q = 0; q < userClasses.length; q++) {
    user_classes.push(userClasses[q].cname.name);
    if (base_classes.indexOf(user_classes[q]) != -1 || user_classes.indexOf("SELF_TYPE") != -1) {
        console.log("ERROR: " + userClasses[q].cname.loc + ": Type-Check: base class redefined");
        process.exit();
    }
}

//combine all classes
var all_classes = base_classes.concat(user_classes);
// Check to make sure there is a main class!
if (user_classes.indexOf("Main") == -1) {
    console.log("ERROR: 0: Type-Check: no Main class BOI");
    //	process.exit();
}

//checks if a user class is redefined
for (var x = 0; x < user_classes.length; x++) {
    for (var y = 0; y < user_classes.length; y++) {
        if (x != y) {
            if (user_classes[x] == user_classes[y]) {
                var index = user_classes.lastIndexOf(user_classes[x]);
                console.log("ERROR: " + userClasses[index].cname.loc + ": Type-Check: class got redefined!");
                process.exit();
            }
        }
    }
}

//check if all inherits are valid
for (var q = 0; q < userClasses.length; q++) {

    //checks all classes that inherits something
    if (userClasses[q].inherit != "") {

        var myinherit = userClasses[q].inherit.name;

        //check from base classes
        if (check(myinherit, ["Int", "String", "Bool"])) {
            console.log("ERROR: " + userClasses[q].inherit.loc + ": Type-Check: cannot inherit from Integer!");
            process.exit();
        }

        //check the undefined classes
        else if (!check(myinherit, all_classes)) {
            console.log("ERROR: " + userClasses[q].inherit.loc + ": Type-Check: inherits from undefined class BOI " + myinherit);
            process.exit();
        }
        //check if a class inherits from itself
        else if (myinherit == userClasses[q].cname.name) {
            console.log("ERROR: 0: Type-Check: I inherit from myself! " + myinherit);
            process.exit();
        }
    }
}


//-------------SECTION 4: outputting expresssion recursively defined below

// A function that will output each expression that can be found
function output_exp(expression, typeflag) {

    var exptype = expression.ekind.etype;
    write("" + expression.eloc + "\n");
    if (typeflag) {
        if (expression.ekind.rettype.substr(0, 9) == "SELF_TYPE") {
            write("SELF_TYPE\n");
        } else {
            write(expression.ekind.rettype + "\n");
        }
    }

    //different write rule for different types of expressions
    if (check(exptype, ["integer", "string"])) {
        write(exptype + "\n" + expression.ekind.value + "\n");
    } else if (exptype == "not") {
        write("not" + "\n")
        output_exp(expression.ekind.value, typeflag);
    } else if (exptype == "bool") {
        write(expression.ekind.value + "\n");
    } else if (exptype == "identifier") {
        write("identifier\n" + expression.ekind.loc + "\n");
        write(expression.ekind.name + "\n");
    } else if (check(exptype, ["lt", "eq", "le", "plus", "minus", "times", "divide"])) {
        write(exptype + "\n");
        output_exp(expression.ekind.val1, typeflag);
        output_exp(expression.ekind.val2, typeflag);
    } else if (check(exptype, ["negate", "isvoid"])) {
        write(exptype + "\n");
        output_exp(expression.ekind.value, typeflag);
    } else if (exptype == "new") {
        write(exptype + "\n");
        write(expression.ekind.value.loc + "\n" + expression.ekind.value.name + "\n");
    } else if (exptype == "self_dispatch") {
        write(exptype + "\n");
        write(expression.ekind.eid.loc + "\n" + expression.ekind.eid.name + "\n" + expression.ekind.args.length + "\n");

        for (var q = 0; q < expression.ekind.args.length; q++) {
            output_exp(expression.ekind.args[q], typeflag);
        }
    } else if (exptype == "if") {
        write(exptype + "\n");
        output_exp(expression.ekind.cond, typeflag);
        output_exp(expression.ekind.itrue, typeflag);
        output_exp(expression.ekind.ifalse, typeflag);
    } else if (exptype == "while") {
        write(exptype + "\n");
        output_exp(expression.ekind.cond, typeflag);
        output_exp(expression.ekind.value, typeflag);
    } else if (exptype == "assign") {
        write(exptype + "\n");
        write(expression.ekind.eid.loc + "\n" + expression.ekind.eid.name + "\n");
        output_exp(expression.ekind.exp, typeflag);

    } else if (exptype == "block") {
        write(exptype + "\n");
        write(expression.ekind.value.length + "\n");
        for (var q = 0; q < expression.ekind.value.length; q++) {
            output_exp(expression.ekind.value[q], typeflag);
        }
    } else if (exptype == "case") {
        write(exptype + "\n");
        output_exp(expression.ekind.exp, typeflag);

        var len = expression.ekind.action.length;
        var mylist = expression.ekind.action;
        write(len + "\n");
        for (var q = 0; q < len; q++) {
            write(mylist[q].id.loc + "\n" + mylist[q].id.name + "\n");
            write(mylist[q].atype.loc + "\n" + mylist[q].atype.name + "\n");
            output_exp(mylist[q].exp, typeflag);
        }
    } else if (exptype == "let") {
        write(exptype + "\n");

        var mylist = expression.ekind.letlist;
        var len = mylist.length;
        write(len + "\n");

        for (var q = 0; q < len; q++) {
            if (mylist[q].exp == "") {
                write("let_binding_no_init\n");
                write(mylist[q].id.loc + "\n" + mylist[q].id.name + "\n");
                write(mylist[q].atype.loc + "\n" + mylist[q].atype.name + "\n");
            } else {
                write("let_binding_init\n");
                write(mylist[q].id.loc + "\n" + mylist[q].id.name + "\n");
                write(mylist[q].atype.loc + "\n" + mylist[q].atype.name + "\n");
                output_exp(mylist[q].exp, typeflag);
            }

        }


        output_exp(expression.ekind.inexp, typeflag);
    } else if (check(exptype, ["static_dispatch", "dynamic_dispatch"])) {
        // console.log(expression.ekind.dtype.name, " I think");
        write(exptype + "\n");
        output_exp(expression.ekind.exp, typeflag);

        if (exptype == "static_dispatch") {
            write(expression.ekind.dtype.loc + "\n" + expression.ekind.dtype.name + "\n");
        }
        write(expression.ekind.did.loc + "\n" + expression.ekind.did.name + "\n");

        var mylist = expression.ekind.arglist;
        var len = mylist.length;
        write(len + "\n");
        for (var q = 0; q < len; q++) {
            output_exp(mylist[q], typeflag);
        }
    }
}

// --------------------SECTION 5: Sort Classes Topologically

var graph = [];
var loners = [];

//build graph
for (var i = 0; i < user_classes.length; i++) {
    var indof = user_classes.indexOf(user_classes[i]);

    if (userClasses[indof].inherit != "") {
        var parent = userClasses[indof].inherit.name;
        var child = userClasses[indof].cname.name;

        if (parent == "SELF_TYPE" || child == "SELF_TYPE") {
            console.log("ERROR: " + userClasses[indof].inherit.loc + ": Type-Check: inheritance cycle there be");
            process.exit();
        }
        graph.push([parent, child]);
    } else {

        //add nodes without dependencies straight to the graph
        var item = userClasses[indof].cname.name;
        loners.push(item);
    }
}

//topologically sort the graph

//catch cycles
if (graph.length > 0) {
    graph = topsort.sortTopo(graph);
    if (graph == "cycle") {
        console.log("ERROR: 0: Type-Check: inheritance cycle there be");
        process.exit();
    }
}

//combine independent nodes and sorted nodes
graph = graph.concat(loners);




//-------------SECTION 6: Inherit Methods and Attributes
for (var ind = 0; ind < graph.length; ind++) {

    // if user class, add relevant methods and attributes
    if (check(graph[ind], user_classes)) {
        var indof = user_classes.indexOf(graph[ind]);
        var attrib = [];
        var attribname = [];
        var method = [];
        var methodname = [];
        var len = userClasses[indof].features.length;

				//loop through attributes and check if they are redefined
        for (var i = 0; i < len; i++) {
            if (userClasses[indof].features[i].fmeth == "Attribute") {
                attrib.push(userClasses[indof].features[i]);

                var newF = userClasses[indof].features[i].fname.name;
                if (attribname.indexOf(newF) == -1) {
                    attribname.push(newF);
                } else {
                    console.log("ERROR: " + userClasses[indof].features[i].fname.loc + ": Type-Check: attribute is redefineed!" + newF);
                    process.exit();
                }
            } else if (userClasses[indof].features[i].fmeth == "Method") {
                method.push(userClasses[indof].features[i]);
                var uniqueFormal = [];
                for (var foind = 0; foind < userClasses[indof].features[i].formals.length; foind++) {
                    var myformal = userClasses[indof].features[i].formals[foind];

                    //check for duplicate formals
                    if (uniqueFormal.indexOf(myformal.fname.name) == -1) {
                        uniqueFormal.push(myformal.fname.name);
                    } else {
                        console.log("ERROR: " + myformal.fname.loc + ": Type-Check: duplicate formals named!! " + myformal.fname.name);
                        process.exit();
                    }
                }

                //check for undefined classes in method signatures
                var rettype = userClasses[indof].features[i].mtype;
                if (all_classes.indexOf(rettype.name) == -1) {
                    if (rettype.name != "SELF_TYPE") {
                        console.log("ERROR: " + rettype.loc + ": Type-Check: method returns undefined type!!" + rettype.name);
                        process.exit();
                    }
                }

                var newF = userClasses[indof].features[i].mname.name;

								//check if methods were incorrectly redefined
                if (methodname.indexOf(newF) == -1) {
                    methodname.push(newF);
                } else {
                    console.log("ERROR: " + userClasses[indof].features[i].mname.loc + ": Type-Check: method was refefined!!" + newF);
                    process.exit();
                }
            }
        }

        //Make sure Main has all required Cool specifications
        if (userClasses[indof].cname.name == "Main") {
            var mind = -1;
            var flag = true;

            for (var i = 0; i < method.length; i++) {
                if (method[i].mname.name == "main") {
                    flag = false;
                    mind = i;
                }
            }
            // if (flag) {
            //     console.log("ERROR: 0: Type-Check: no main method in Main class BOI");
            //     process.exit();
            // }

            if (mind != -1 && method[mind].formals.length != 0) {
                console.log("ERROR: 0: Type-Check: main method should have no formals");
                process.exit();
            }

        }

        //make sure self and SELF_TYPE are used properly
        for (var mine = 0; mine < attrib.length; mine++) {
            var checktype = attrib[mine].ftype;
            var checktheatt = attrib[mine].fname;
            if (checktheatt.name == "self") {
                console.log("ERROR: " + checktheatt.loc + ": Type-Check: attribute named self!!");
                process.exit();
            }

            if (checktheatt.name == "SELF_TYPE") {
                console.log("ERROR: " + checktheatt.loc + ": Type-Check: attribute named SELF_TYPE!!");
                process.exit();
            }
        }

				//set the methods and attributes of user class
        userClasses[indof].attrib = attrib;
        userClasses[indof].method = method;




        //if class DOES inherit, add the relevant features
        if (userClasses[indof].inherit != "") {

            var parent = userClasses[indof].inherit.name;
            var pind = user_classes.indexOf(parent);


            // if the parent is a USER CLASS
            if (pind != -1) {

                var list1 = userClasses[pind].attrib;
                var list2 = userClasses[indof].attrib;

                //parent: add all methods
                var meth1 = [];
                for (var i = 0; i < userClasses[pind].method.length; i++) {
                    meth1.push(userClasses[pind].method[i]);
                }

                //child: add al methods
                var meth2 = []
                for (var i = 0; i < userClasses[indof].method.length; i++) {
                    meth2.push(userClasses[indof].method[i]);
                }

								//combine their functions
                userClasses[indof].attrib = list1.concat(list2);

								//loop through parent and kid functions and remove overriden methods
                for (var par = 0; par < meth1.length; par++) {
                    for (var kid = 0; kid < meth2.length; kid++) {

                        //checks adherence to the Cool formal specifications
                        if (meth2[kid].mname.name == meth1[par].mname.name) {
                            if (!arraysEqual(meth2[kid].formals, meth1[par].formals)) {
                                console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: bad redefined formals!! " + meth2[kid].mname.name);
                                process.exit();

                            }
                            if (meth2[kid].mtype.name != meth1[par].mtype.name) {
                                console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: refefines method bad return type!! " + meth2[kid].mname.name);
                                process.exit();
                            }

														//removes parent's method and moves the function up to maintain correct order
                            meth1[par] = meth2[kid];
                            meth2.splice(kid, 1);
                            kid--;

                        }
                    }
                }

                userClasses[indof].method = meth1.concat(meth2);


                //checks for Cool attribute rules
                var attinname = [];
                for (var t = 0; t < userClasses[indof].attrib.length; t++) {
                    var checkatt = userClasses[indof].attrib[t].fname.name;
                    if (attinname.indexOf(checkatt) == -1) {
                        attinname.push(checkatt);
                    } else {
                        console.log("ERROR: " + userClasses[indof].attrib[t].fname.loc + ": Type-Check: attribute is redefineed!" + checkatt);
                        process.exit();
                    }
                }
            }
            // ELSE the parent is IO or Object
            else {
                //Make sure built in classes aren't redefined
                if (base_classes.indexOf(parent) != -1) {
                    if (parent == "IO") {
                        var meth1 = [];


                        meth1.push(methabort);
                        meth1.push(copy);
                        meth1.push(type_name);
                        meth1.push(in_int);
                        meth1.push(in_string);
                        meth1.push(out_int);
                        meth1.push(out_string);


                        var meth2 = userClasses[indof].method;

                        //check for redefined formals
                        for (var par = 0; par < meth1.length; par++) {
                            for (var kid = 0; kid < meth2.length; kid++) {
                                if (meth2[kid].mname.name == meth1[par].mname.name) {
                                    if (!arraysEqual(meth2[kid].formals, meth1[par].formals)) {
                                        console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: bad redefined formals!! " + meth2[kid].mname.name);
                                        process.exit();
                                    }
                                    if (meth2[kid].mtype.name != meth1[par].mtype.name) {
                                        console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: refefines method bad return type!! " + meth2[kid].mname.name);
                                        process.exit();
                                    }

                                    //swap parent and child and delete child
                                    meth1[par]= meth2[kid];
                                    meth2.splice(kid, 1);
                                    kid--;

                                }
                            }
                        }
                        var temp = userClasses[indof].method;
                        userClasses[indof].method = meth1.concat(temp);
                    }

                    //check for redefining Object's methods
                    else if (parent == "Object") {
                        var meth1 = [];
                        meth1.push(methabort);
                        meth1.push(copy);
                        meth1.push(type_name);

                        var meth2 = userClasses[indof].method;
                        for (var par = 0; par < meth1.length; par++) {
                            for (var kid = 0; kid < meth2.length; kid++) {

															//kid and parent method have the same name
                                if (meth2[kid].mname.name == meth1[par].mname.name) {
																	//poorly redefined formals
                                    if (!arraysEqual(meth2[kid].formals, meth1[par].formals)) {
                                        console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: bad redefined formals!! " + meth2[kid].mname.name);
                                        process.exit();

                                    }
																		//redefines method return type
                                    if (meth2[kid].mtype.name != meth1[par].mtype.name) {
                                        console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: refefines method bad return type!! " + meth2[kid].mname.name);
                                        process.exit();
                                    }

																		//swap parent and child and delete child
                                    meth1[par]= meth2[kid];
                                    meth2.splice(kid, 1);
                                    kid--;
                                }

																//set the origin to object
																else {
                                    meth1[par].definition = "Object";

                                }
                            }
                        }

												//add objects functions
                        var temp = userClasses[indof].method;
                        userClasses[indof].method = meth1.concat(temp);
                    }
                } else {
                    console.log("ERROR: 0: Type-Check: inherits from undeclared?");
                }
            }
        } else {

            //if a class doesn't inherit anything else, it inherits from Object by default
            var meth1 = [];
            meth1.push(methabort);
            meth1.push(copy);
            meth1.push(type_name);


						//loop through and check for overridden methods
            var meth2 = userClasses[indof].method;
            for (var par = 0; par < meth1.length; par++) {
                for (var kid = 0; kid < meth2.length; kid++) {
                    if (meth2[kid].mname.name == meth1[par].mname.name) {
                        if (!arraysEqual(meth2[kid].formals, meth1[par].formals)) {
                            console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: bad redefined formals!! " + meth2[kid].mname.name);
                            process.exit();
                        }
                        if (meth2[kid].mtype.name != meth1[par].mtype.name) {
                            console.log("ERROR: " + meth2[kid].mname.loc + ": Type-Check: refefines method bad return type!! " + meth2[kid].mname.name);
                            process.exit();
                        }
                        //swap parent and child and delete child
                        meth1[par]= meth2[kid];
                        meth2.splice(kid, 1);
                        kid--;

                    } else {
                        meth1[par].definition = "Object";
                    }
                }
            }
            var temp = userClasses[indof].method;
            userClasses[indof].method = meth1.concat(temp);
        }
    }
}






//------------------------------------SECTION 7: Type Check Expression

//also sets their ACTUAL return type after typechecking them
function tcheckExp(expre, classname, objsym, metsym) {
    var expid = expre.ekind.etype;


    switch (expid) {
        case "integer":
            expre.ekind.rettype = "Int";
            break;
        case "string":
            expre.ekind.rettype = "String";
            break;
        case "bool":
            expre.ekind.rettype = "Bool";
            break;
        case "minus":
        case "times":
        case "divide":
        case "plus":
						//type check both expressions for arithmetic
            tcheckExp(expre.ekind.val1, classname, objsym, metsym);
            tcheckExp(expre.ekind.val2, classname, objsym, metsym);

						//make sure their actual return type is Int
            if (expre.ekind.val1.ekind.rettype == "Int" && expre.ekind.val2.ekind.rettype == "Int") {
                expre.ekind.rettype = "Int";
            } else {
                console.log("ERROR: " + expre.eloc + ": Type-Check: arithmetic failed typchecking - yo");
                process.exit();
            }
            break;
        case "le":
        case "lt":
        case "eq":
					//type check both expressions for comparisons
            tcheckExp(expre.ekind.val1, classname, objsym, metsym);
            tcheckExp(expre.ekind.val2, classname, objsym, metsym);

						//if they are Int, String, or Bool make sure they have the same return type
            if (check(expre.ekind.val1.ekind.rettype, ["Int", "String", "Bool"]) || check(expre.ekind.val2.ekind.rettype, ["Int", "String", "Bool"]))
                if (expre.ekind.val1.ekind.rettype == expre.ekind.val2.ekind.rettype) {
                    expre.ekind.rettype = "Bool";
                }
            else {
                console.log("ERROR: " + expre.eloc + ": Type-Check: equals failed typchecking");
                process.exit();
            }
            expre.ekind.rettype = "Bool";
            break;
        case "isvoid":
            tcheckExp(expre.ekind.value, classname, objsym, metsym);
            expre.ekind.rettype = "Bool";
            break;
        case "negate":
            tcheckExp(expre.ekind.value, classname, objsym, metsym);
            if (expre.ekind.value.ekind.rettype != "Int") {
                console.log("ERROR: " + expre.ekind.value.eloc + ": Type-Check: negate failed typchecking");
                process.exit();
            }
            expre.ekind.rettype = "Int";
            break;
        case "not":
            tcheckExp(expre.ekind.value, classname, objsym, metsym);
            if (expre.ekind.value.ekind.rettype != "Bool") {
                console.log("ERROR: " + expre.ekind.value.eloc + ": Type-Check: not failed typchecking");
                process.exit();
            }
            expre.ekind.rettype = "Bool";
            break;
        case "new":
            var mytype = expre.ekind.value.name;

						//check of type is SELF TYPE
            if (mytype.substr(0, 9) == "SELF_TYPE") {
							//set selftype subclass
                expre.ekind.rettype = "SELF_TYPE|" + classname;
            } else {
                expre.ekind.rettype = mytype;
            }
            break;
        case "assign":

            var maxtype = objsym.find(classname).find(expre.ekind.eid.name);
            tcheckExp(expre.ekind.exp, classname, objsym, metsym);
            var mytype = expre.ekind.exp.ekind.rettype;

						//exact type requested
            if (maxtype == mytype) {
                expre.ekind.rettype = mytype;
            } else {
							//is a subtype of requested type
                if (checkInherit(maxtype, mytype)) {
                    expre.ekind.rettype = mytype;
                } else {
									//type does not conform, throw error
                    console.log("ERROR: " + expre.eloc + ": Type-Check: assign is taihen checking failed typchecking");
                    process.exit();
                }
            }
            break;
        case "identifier":
				//if self type, set sub class and stop
            if (expre.ekind.name == "self") {
                expre.ekind.rettype = "SELF_TYPE|" + classname;
                break;
            }
						//otherwise make sure the variable exists/ is in scope
            // console.log("ident", objsym.find(classname));
            if (objsym.find(classname).find(expre.ekind.name) != "I didn't find anything master") {
                expre.ekind.rettype = objsym.find(classname).find(expre.ekind.name);
            } else {
                console.log("ERROR: " + expre.eloc + ": Type-Check: ideintifier checking failed typchecking");
                process.exit();
            }
            break;
        case "if":
            			// typechecking the condition expression of the if e then e else e fi
			tcheckExp(expre.ekind.cond, classname, objsym, metsym);


			// grabbing the type of the condition for ease of understanding how the program
			// reflects the typechecing rules
			// also typechecking the other expressions
            var condi = expre.ekind.cond.ekind.rettype;
            tcheckExp(expre.ekind.itrue, classname, objsym, metsym);
            tcheckExp(expre.ekind.ifalse, classname, objsym, metsym);
            var t2 = expre.ekind.itrue.ekind.rettype;
            var t3 = expre.ekind.ifalse.ekind.rettype;


			// if the condition is not a boolean it's pointless to continue
            if (condi != "Bool") {
                console.log("ERROR: " + expre.eloc + ": Type-Check: if condition not Booleantype");
                process.exit();
            }

			// calling the least-common-ancestor or LUB function
			// to get the right return type for if
            var lCA = leastCommonAncestor(t2, t3);
            expre.ekind.rettype = lCA;

            break;
        case "block":

			// setup a variable to store the last type of the expressions in the block
			// since the return type is just the last type
            var last_type = "";

			// typecheck all of the expressions in the block
            for (var q = 0; q < expre.ekind.value.length; q++) {
                tcheckExp(expre.ekind.value[q], classname, objsym, metsym);
                last_type = expre.ekind.value[q].ekind.rettype;
            }

			// set the return type to the variable we setup in the beginning
      // console.log("ret type: ",expre.ekind);
            expre.ekind.rettype = last_type;
            break;
        case "while":

			// similar to IF, typecheck the condition expression and store it into a variable
            tcheckExp(expre.ekind.cond, classname, objsym, metsym);
            var condi = expre.ekind.cond.ekind.rettype;

			// if the condition is not a boolean, it's pointless to continue
            if (condi != "Bool") {
                console.log("ERROR: " + expre.ekind.cond.eloc + ": Type-Check: while condition not Booleantype");
                process.exit();
            }

			// if there was no problem, typecheck the part that will loop
            tcheckExp(expre.ekind.value, classname, objsym, metsym);

			// as per the typing rules, hardcode the returntype to Object
            expre.ekind.rettype = "Object";
            break;
        case "case":
			// typecheck the variable that is being switched upon
			tcheckExp(expre.ekind.exp, classname, objsym, metsym);

			// create variables to store the types that appear in the case
			// and the various branches taken by the case statement
			// 	so that no branch is repeated
            var types = [];
            var branch_types = [];

			// handle each branch of the case
            for (var q = 0; q < expre.ekind.action.length; q++) {

				// if the branch is a repeat have an error message
				// otherwise push it onto the list of branches that have been seen
                if (branch_types.indexOf(expre.ekind.action[q].atype.name) == -1) {
                    branch_types.push(expre.ekind.action[q].atype.name);
                } else {
                    console.log("ERROR: " + expre.ekind.action[q].id.loc + ": Type-Check: repeated branches of case");
                    process.exit();
                }

				// SELF_TYPE cannot be a branch of case
                if (expre.ekind.action[q].atype.name.substr(0, 9) == "SELF_TYPE") {
                    console.log("ERROR: " + expre.ekind.action[q].atype.loc + ": Type-Check: self-type as a branch of case");
                    process.exit();
                }

                // push the identifier on the branch onto the object symbol tree so that expression can use it
				// and typecheck the expression
				// and store the retun type of the expression
                objsym.find(classname).add(expre.ekind.action[q].id.name, expre.ekind.action[q].atype.name);
                tcheckExp(expre.ekind.action[q].exp, classname, objsym, metsym);
                objsym.find(classname).remove(expre.ekind.action[q].id.name);
                types.push(expre.ekind.action[q].exp.ekind.rettype);
            }

            // have a list of types, find the final LUB to return
            // console.log("types",types);
            if(types.length ==1){
              basecase= types[0];
            }
            else{
              var basecase = leastCommonAncestor(types[0], types[1]);
              for (var i = 1; i < types.length; i++) {
                  basecase = leastCommonAncestor(basecase, types[i]);
              }
            }


			// and set the return type
            expre.ekind.rettype = basecase;

            break;
        case "dynamic_dispatch":
            // type is undefined or null, because it's not static!!
            tcheckExp(expre.ekind.exp, classname, objsym, metsym);
            var t0 = expre.ekind.exp.ekind.rettype;
            var theMethod;

			// following the type checking rules, if t0 is self-type then grab the C
			// to search that class's method tree for the right method's return types
            if (t0.substr(0, 9) == "SELF_TYPE") {
                var thistype = t0.substr(10, t0.length);
                if (thistype.length <= 0) {
					// if its just this type we search the current class's tree
                    theMethod = metsym.find(classname).find(expre.ekind.did.name);

                } else {
					// otherwise search the other class's tree
                    theMethod = metsym.find(thistype).find(expre.ekind.did.name);
                }

            } else {
				// if it's not self-type, just find the method in t0's tree
                theMethod = metsym.find(t0).find(expre.ekind.did.name);
            }

			// Our symbol tree returns this string if the item maps to nothing in the tree
			// rather than handling the symbol tree throwing anything
            if (theMethod == "I didn't find anything master") {
                console.log("ERROR: " + expre.eloc + ": Type-Check: dynamic dispatch error not existing!!");
                process.exit();

            }

			// final actual return type is the last item in the found list
			var finalOne = theMethod[theMethod.length - 1];

          	// check each of the parameters and typecheck them
			// and check that aginst the supposed type
			// if they're not subtypes, then an error
            if (expre.ekind.arglist.length == (theMethod.length - 1)) {
                for (var q = 0; q < theMethod.length - 1; q++) {

					// typecheck the argument
                    tcheckExp(expre.ekind.arglist[q], classname, objsym, metsym);
                    var ret = expre.ekind.arglist[q].ekind.rettype;

					// if it's self-type check the "sub_C"
                    if (ret.substr(0, 9) == "SELF_TYPE") {
                        var length = ret.length;
                        if (!checkInherit(theMethod[q], expre.ekind.arglist[q].ekind.rettype.substr(10, length))) {
                            console.log("ERROR: " + expre.eloc + ": Type-Check: dynamic dispatch error bad formals!!");
                            process.exit();
                        }
                    } else if (!checkInherit(theMethod[q], expre.ekind.arglist[q].ekind.rettype)) {

						// if it's not self-type just check if the classes are subclasses
                        console.log("ERROR: " + expre.eloc + ": Type-Check: dynamic dispatch error bad formals!!");
                        process.exit();
                    }
                }
            } else {
				// if the two lists don't have the same length, it can't be a correct method call
                console.log("ERROR: " + expre.eloc + ": Type-Check: dynamic dispatch error yo");
                process.exit();
            }

			// if the method is supposed to return "SELF_TYPE"
			// set the rettype to t0, else use the last item in the symbol table's list
			if (finalOne == "SELF_TYPE") {
                expre.ekind.rettype = t0;

            } else {
                expre.ekind.rettype = finalOne;
            }
            break;
        case "self_dispatch":

			// get the method specified by the dispatch
            var theMethod = metsym.find(classname).find(expre.ekind.eid.name);

			// if the symbol table returns that if found nothing
			if (theMethod == "I didn't find anything master") {
                console.log("ERROR: " + expre.eloc + ": Type-Check: self dispatch error not existing!!");
                process.exit();

            }

			// set a variable for the actual return type of the method
			var finalOne = theMethod[theMethod.length - 1];

			// if the formal and passed in paramters do not have the same number of elements
			// then it's not ok, and error
            if (expre.ekind.args.length == theMethod.length - 1) {
                for (var q = 0; q < theMethod.length - 1; q++) {

					// check each arguement passed in
                    tcheckExp(expre.ekind.args[q], classname, objsym, metsym);

					// if the actual and supposed types are not parent and child, there is an error
                    if (!checkInherit(theMethod[q], expre.ekind.args[q].ekind.rettype)) {
                      if(expre.ekind.args[q].ekind.rettype.substr(0, 9) == "SELF_TYPE"){
                        var rettypelen = expre.ekind.args[q].ekind.rettype.length;
                          if(!checkInherit(theMethod[q], expre.ekind.args[q].ekind.rettype.substr(10, rettypelen))){
                            console.log("ERROR: " + expre.eloc + ": Type-Check: self dispatch error bad formals!!");
                            process.exit();
                          }
                      }
                      else{
                        console.log("ERROR: " + expre.eloc + ": Type-Check: self dispatch error bad formals!!");
                        process.exit();
                      }
                    }
                }
            } else {
                console.log("ERROR: " + expre.eloc + ": Type-Check: self dispatch error yo");
                process.exit();
            }

			// if it's gotten this far it's ok, set the return type to the specified return type
			expre.ekind.rettype = finalOne;
            break;
        case "let":
			// t2 is the final return type of the let
			// thingsleton will be a list of the identifiers pushed onto the object symbol table
            var t2 = "";
            var thingsleton = [];

            // for each item of the stringed together let, check stuff
			// will only run once if there is only one item
            for (var i = 0; i < expre.ekind.letlist.length; i++) {

                //no init let binding
                if (expre.ekind.letlist[i].exp == 'undefined' || expre.ekind.letlist[i].exp == "") {

					// if it's defined type is SELF_TYPE, handle it
					//  by giving the SELF_TYPE a subtype_C -> SELF_TYPE|SUBC
                    var t0 = expre.ekind.letlist[i].atype.name;
                    if (t0 == "SELF_TYPE") {
                        t0 = "SELF_TYPE|" + classname;
                    }

					// then push this onto the object symbol table
					// and add the name to a list of the names so we know later
                    objsym.find(classname).add(expre.ekind.letlist[i].id.name, t0);
                    thingsleton.push(expre.ekind.letlist[i].id.name);
                }
                //init let therefore it has AN attached exp
                else {
                    // store the stated return type, and handle if it's SELF_TYPE
                    var t0 = expre.ekind.letlist[i].atype.name;
                    if (t0 == "SELF_TYPE") {
                        t0 = "SELF_TYPE|" + classname;
                    }

					// typecheck said attached expression, store this into t1
                    tcheckExp(expre.ekind.letlist[i].exp, classname, objsym, metsym);
                    var t1 = expre.ekind.letlist[i].exp.ekind.rettype;

					// if the expression returns SELF_TYPE, ensure it inherits correctly from the suppsoed return type
                    if (t1 == "SELF_TYPE") {
                        if (checkInherit(t0, t1)) {
                            // t0 = "SELF_TYPE";
                        } else {
                            console.log("ERROR: " + expre.eloc + ": Type-Check: self type let error");
                            process.exit();
                        }
                    }

					// otherwise, or if there were no other errors, check their inheritance stance
                    if (checkInherit(t0, t1)) {

						// if the inheritance is ok, push it onto the symbol table and make note of t
                        objsym.find(classname).add(expre.ekind.letlist[i].id.name, t0);
                        thingsleton.push(expre.ekind.letlist[i].id.name);
                    } else {
                        // else, error

                        var t1len = t1.length;
                        if(!checkInherit(t0, t1.substr(10,t1len))){
                        //  console.log(t0, t1.substr(10, t1len));
                         console.log("ERROR: " + expre.eloc + ": Type-Check: let expression error yo t0, t1");
                         process.exit();
                       }
                        // console.log("ERROR: " + expre.eloc + ": Type-Check: let expression error yo t0, t1");
                        // process.exit();
                    }
                }
            }

			// after all the lets have been pushed on, typecheck the "in exp" expression
			// store this return type in t2
            tcheckExp(expre.ekind.inexp, classname, objsym, metsym);
            t2 = expre.ekind.inexp.ekind.rettype;

			// remove all of the objects pushed onto the object symbol table
            for (var i = 0; i < expre.ekind.letlist.length; i++) {
                objsym.find(classname).remove(thingsleton[i])
            }

			// and set the final return type!
            expre.ekind.rettype = t2;
            break;
        case "static_dispatch":
			//typecheck the expression that this is called on and store the return type in eo
            tcheckExp(expre.ekind.exp, classname, objsym, metsym);
            var eo = expre.ekind.exp.ekind.rettype;

			// check if eo is valid given the type passed in as what should be called
            if (checkInherit(expre.ekind.dtype.name, eo)) {
                var theMethod = metsym.find(expre.ekind.dtype.name).find(expre.ekind.did.name);

				// if there was no method to be called in the method symbol table
                if (theMethod == "I didn't find anything master") {
                    console.log("ERROR: " + expre.eloc + ": Type-Check: static dispatch error not existing!!");
                    process.exit();
                }

				// store the supposed return type
                var finalOne = theMethod[theMethod.length - 1];

				// check all of the formals
                if (expre.ekind.arglist.length == theMethod.length - 1) {
                    for (var q = 0; q < theMethod.length - 1; q++) {

						// check the expression of the argument passed in
                        tcheckExp(expre.ekind.arglist[q], classname, objsym, metsym);

						// if it's return type is not a vlid subtype of what it says is being passed in
						// there is an error with the formals
                        if (!checkInherit(theMethod[q], expre.ekind.arglist[q].ekind.rettype)) {
                          if(expre.ekind.arglist[q].ekind.rettype.substr(0, 9) == "SELF_TYPE"){
                            var rettypelen = expre.ekind.arglist[q].ekind.rettype.length;
                            if(!checkInherit(theMethod[q], expre.ekind.arglist[q].ekind.rettype.substr(10, rettypelen))){
                              console.log("ERROR: " + expre.eloc + ": Type-Check: static dispatch error bad formals!!");
                              process.exit();
                            }

                          }else{
                            console.log("ERROR: " + expre.eloc + ": Type-Check: static dispatch error bad formals!!");
                            process.exit();
                          }
                        }
                    }
                }
				// if check, there are the wrong number of parameters for the method called
				else {
                    console.log("ERROR: " + expre.eloc + ": Type-Check: static dispatch error wrong # params");
                    process.exit();
                }
            }
			// static dispatch error, the eo and T0 put in are incompatible
			else {
                console.log("ERROR: " + expre.eloc + ": Type-Check: static dispatch error yo");
                process.exit();
            }

			// if the specified return type is SELF_TYPE, retur eo, otherwise set it to the expected return type
            if (finalOne == "SELF_TYPE") {
                expre.ekind.rettype = eo;
            } else {
                expre.ekind.rettype = finalOne;
            }

            break;
        default:
            // sanity check, this should never happen
            expre.rettype = "DIVYA DON'T COMMENT ME OUT";
    }
}

//checking methods, a method, classname, object and method symbol tree are being passed in
function tcheckMeth(mymeth, classname, objsym, metsym) {
    var supposedRet = mymeth.mtype.name;

    // add the allowable parameters to the object symbol table
    for (var q = 0; q < mymeth.formals.length; q++) {
        objsym.find(classname).add(mymeth.formals[q].fname.name, mymeth.formals[q].ftype.name);
    }

    // type-checks the body of the expression
    tcheckExp(mymeth.mbody, classname, objsym, metsym);
    var actualRet = mymeth.mbody.ekind.rettype;


	// check if the actual return type of the body was self-type
  // console.log("actual ret",mymeth.mbody.ekind);
    if (actualRet.substr(0, 9) == "SELF_TYPE") {
        // then supposedRet MIGHT be self type
        // and it might be not self-type
        if (supposedRet.substr(0, 9) == "SELF_TYPE") {
            // if it is self-type everything is ok

        } else {
            // in the case actual is SELF and supposed is not
			// need to check if the actual return's sub_C is a subtype of the supposed return type

			if (!checkInherit(supposedRet, actualRet.substr(10, actualRet.length))) {
                console.log("ERROR: " + mymeth.mname.loc + ": Type-Check: method issue child self-type only!");
                process.exit();
            }
        }

    }
	// otherwise just check if they're substypes
	else if (!checkInherit(supposedRet, actualRet)) {
        console.log("ERROR: " + mymeth.mname.loc + ": Type-Check: method issue!");
        process.exit();

    }

	// pop off method parameters from the object symbol table
	for (var q = 0; q < mymeth.formals.length; q++) {
        objsym.find(classname).remove(mymeth.formals[q].fname.name, mymeth.formals[q].ftype.name);
    }
}

// typechecks an atrribute
function tcheckAtt(myatt, classname, objsym, metsym) {

	// store the name of the attribute
    var aname = myatt.fname.name;

	// if it is an attribute with no initial value
    if (myatt.finit.length < 1) {
        myatt.rettype = objsym.find(classname).find(myatt.fname.name);
        // whoo no typechecking to do!
    } else {
		// otherwise typecheck the inital expression
		tcheckExp(myatt.finit, classname, objsym, metsym);

        var bodytype = myatt.finit.ekind.rettype;
        var blen = bodytype.length;
        if(bodytype.substr(0,9)=="SELF_TYPE"){
          if(objsym.find(classname).find(aname)=="SELF_TYPE"){
            bodytype = "SELF_TYPE";
          }
          else{
            bodytype= bodytype.substr(10,blen);

          }
        }
		// ensure that the type actually returned by the expression is a subtype of the stated type
        if (checkInherit(objsym.find(classname).find(aname), bodytype) || objsym.find(classname).find(aname) == bodytype) {

        }
        else{
          // if it's not throw an error
          console.log("ERROR: " + myatt.fname.loc + ": Type-Check: i!!!!", bodytype, "should be", objsym.find(classname).find(aname));
          process.exit();
        }

    }
}

// type checks an entire class
function tcheckClass(mclass, classname, objsym, metsym) {
    // I am given a symbol table, and a user-class
    var className = mclass.cname.name;

	// loop through all of the methods belonging to the class and typecheck them
    for (var i = 0; i < mclass.method.length; i++) {

        tcheckMeth(mclass.method[i], classname, objsym, metsym);
    }
	// loop through all of the attributes and typecheck them too
    for (var i = 0; i < mclass.attrib.length; i++) {

        tcheckAtt(mclass.attrib[i], classname, objsym, metsym);
    }

}

// set up two symbol tables, one for all the objecs and one for the methods
var osym = new symboltable.SymbolTable();
var msym = new symboltable.SymbolTable();

// loop through the classes and add all necessary things to tables
for (ind in all_classes) {
    var clname = all_classes[ind];
    if (user_classes.indexOf(clname) != -1) {

        // this is a user class
        var indof = user_classes.indexOf(clname);
        var myClass = userClasses[indof];

		// make the table map to a table with the classname as a key
        osym.add(clname, new symboltable.SymbolTable());
        msym.add(clname, new symboltable.SymbolTable());

		// actually add values to the table within the table
        for (var i = 0; i < myClass.attrib.length; i++) {
            osym.find(clname).add(myClass.attrib[i].fname.name, myClass.attrib[i].ftype.name);
        }
        for (var i = 0; i < myClass.method.length; i++) {
            var methformals = [];
            for (var q = 0; q < myClass.method[i].formals.length; q++) {
                methformals.push(myClass.method[i].formals[q].ftype.name);
            }
            methformals.push(myClass.method[i].mtype.name);
            msym.find(clname).add(myClass.method[i].mname.name, methformals);

        }

    } else {
        // this is a basic class

		// again, make the table map to another table
        osym.add(clname, new symboltable.SymbolTable());
        msym.add(clname, new symboltable.SymbolTable());

        // everything gets the object's methods
        msym.find(clname).add("abort", ["Object"]);
        msym.find(clname).add("copy", ["SELF_TYPE"]);
        msym.find(clname).add("type_name", ["String"]);

		// strings have three built in methods
        if (clname == "String") {
            msym.find(clname).add("length", ["Int"]);
            msym.find(clname).add("concat", ["String", "String"]);
            msym.find(clname).add("substr", ["Int", "Int", "String"]);
        } else if (check(clname, ["Int", "Bool"])) {
            // just get all of object's stuff
			// none of their own
        } else if (clname == "IO") {

			// IO gets four built in methods
            msym.find(clname).add("out_string", ["String", "SELF_TYPE"]);
            msym.find(clname).add("out_int", ["Int", "SELF_TYPE"]);
            msym.find(clname).add("in_string", ["String"]);
            msym.find(clname).add("in_int", ["Int"]);

        } else {
			// ensure that everything is caught
        }
    }

}

// call the above typechecking methods on all of the classes!
for (ind in all_classes) {

	// if the class is a user class, typecheck it
    if (check(all_classes[ind], user_classes)) {
        var indof = user_classes.indexOf(all_classes[ind]);

        // pass in the tymbol tables! Should be nothing in it!
        tcheckClass(userClasses[indof], user_classes[indof], osym, msym);

    }
}


//----------------------- SECTION 8: Create File to Print: Class Map, Imp Map, Parent Map, AST
// sort the classes sometime before they get output
all_classes.sort();
// setupt the files to be printed to
writeFirst("");


// write "class map", ensure the index begins at zero
write("class_map\n" + all_classes.length + "\n");
var ind = 0;

// loop through the classes and write about each
for (ind in all_classes) {
    write("" + all_classes[ind] + "\n");

	// if the class if a user class, print its methods
    if (check(all_classes[ind], user_classes)) {

		// get the current class, and the length of it's attributes
        var indof = user_classes.indexOf(all_classes[ind]);
        write(userClasses[indof].attrib.length + "\n");

		// write each attribute
        for (var i = 0; i < userClasses[indof].attrib.length; i++) {
			// determine if has an init or not to print
            if (userClasses[indof].attrib[i].finit != "") {
                write("initializer\n" + userClasses[indof].attrib[i].fname.name + "\n" + userClasses[indof].attrib[i].ftype.name + "\n");
                output_exp(userClasses[indof].attrib[i].finit, true);
            } else {
                write("no_initializer\n" + userClasses[indof].attrib[i].fname.name + "\n" + userClasses[indof].attrib[i].ftype.name + "\n");
            }
        }
    } else {
		// else it's a base class, no attributes
        write("0\n");
    }
}

// finish the class mapping


// this function will do the implemenation map for base classes
// terribly hardcoded, but functional.
function baseinheritObject(intype) {
    // set up a array of methods that will belong to a base class
	var meth1 = [];

	// string gets object and string
    if (intype == "String") {
        meth1.push(methabort);
        meth1.push(copy);
        meth1.push(type_name);

        meth1.push(meconcat);
        meth1.push(melen);
        meth1.push(mesubs);
	}
	// IO gets Object and IO
	else if (intype == "IO") {
        meth1.push(methabort);
        meth1.push(copy);
        meth1.push(type_name);
        meth1.push(in_int);
        meth1.push(in_string);
        meth1.push(out_int);
        meth1.push(out_string);

    }
	// Int, Bool, Object just get object
	else {
        meth1.push(methabort);
        meth1.push(copy);
        meth1.push(type_name);
    }

	// print the length of the method array
    write(meth1.length + "\n");
    for (var i = 0; i < meth1.length; i++) {

		// for sanity, create a vatiable to store the current method
        var internalmeth = meth1[i];
        write(internalmeth.mname.name + "\n");
        write(internalmeth.formals.length + "\n");
        // write formal names, and the length of the formals
        for (var q = 0; q < internalmeth.formals.length; q++) {
            write(internalmeth.formals[q].fname.name + "\n");
        }

		// write where the class was defined, where the body starts, the method name
		// and internal since these are all base classes
        write(internalmeth.definition + "\n");
        write(internalmeth.mbody.eloc + "\n");
        write(internalmeth.mtype.name + "\n");
        write("internal\n");
        write(internalmeth.definition + "." + internalmeth.mname.name + "\n");

    }
}

// begin writeing the implementation map
write("implementation_map\n" + all_classes.length + "\n");
var ind = 0;
// ensure the index begins at zero, and loop through all of the classes
for (ind in all_classes) {
    write("" + all_classes[ind] + "\n");

	// if the class if a user class
    if (check(all_classes[ind], user_classes)) {
        var indof = user_classes.indexOf(all_classes[ind]);

		// print the length of the method array
        write(userClasses[indof].method.length + "\n");

        // Go through all of the methods in a class
        for (var i = 0; i < userClasses[indof].method.length; i++) {
            // write the method name
            // write the number of formals and then the actual formal names
            write(userClasses[indof].method[i].mname.name + "\n");
            write(userClasses[indof].method[i].formals.length + "\n");

            for (var q = 0; q < userClasses[indof].method[i].formals.length; q++) {
                write(userClasses[indof].method[i].formals[q].fname.name + "\n");
            }
            //Check if this is an internal methid
            if (check(userClasses[indof].method[i].definition, base_classes)) {
                var internalmeth = userClasses[indof].method[i];

				// write a bunch of information if it's an internal method
                write(internalmeth.definition + "\n");
                write(internalmeth.mbody.eloc + "\n");
                write(internalmeth.mtype.name + "\n");
                write("internal\n");
                write(internalmeth.definition + "." + internalmeth.mname.name + "\n");


            }
            // this is not an internal method, so do other stuff
            else {
               	// just write where it's defined and the body
				// with the flag true to print the type
                write(userClasses[indof].method[i].definition + "\n");
                output_exp(userClasses[indof].method[i].mbody, true);

            }
            // for loop for all the methds!
        }
    } else {
		// this is a base class, so go to the before-mentioned function
		baseinheritObject(all_classes[ind]);
        // write("0\n");
    }
}

// write the parent map
write("parent_map\n" + (all_classes.length - 1) + "\n");

// loop through all of the classes again
for (ind in all_classes) {
    var indof = user_classes.indexOf(all_classes[ind]);
    if (indof == -1) {
        // this is a base class - therefore inherits object
		// print as long as it's not actually the object class
        if (all_classes[ind] != "Object") {
            write(all_classes[ind] + "\n");
            write("Object\n");
        }
    } else {
        // this is a user class, it either inherits a thing
        // or it inherits object
        write(all_classes[ind] + "\n");
        if (userClasses[indof].inherit == "") {
            // it inherits nothing
            write("Object\n");

        } else {
			// if inherits something, write what it inherits
            write(userClasses[indof].inherit.name + "\n");
        }
    }
}

//------------------SECTION: NEXT ANNOTATED AST

// go through user classes
// and use their features which are a mix of methods and attributes

// helper function to output things that look like identifiers
// have a location and a name
function outputID(identifier) {
    write(identifier.loc + "\n");
    write(identifier.name + "\n");

}

// write the number of classes, and then start looping through them
// this array is the order they were read in
write(user_classes.length + "\n");
for (ind in user_classes) {
	// ge the actual current class instead of the string-stand in
    var thisClass = userClasses[ind];

	// output the class name/location and whether or not it inherits
    outputID(thisClass.cname);
    if (thisClass.inherit != "") {
        write("inherits\n");
        outputID(thisClass.inherit);
    } else {
        write("no_inherits\n");
    }

	// write the number of features, and then loop through all of them to print them
    write(thisClass.features.length + "\n");
    for (var i = 0; i < thisClass.features.length; i++) {

		// get this feature and determine if it is an attribute or a method
        var myFeat = thisClass.features[i];

		// if it's an attribute it either is init, or no init
        if (myFeat.fmeth == "Attribute") {
            if (myFeat.finit == "") {

				// if it has no init print the name and type as identifiers
                write("attribute_no_init\n");
                outputID(myFeat.fname);
                outputID(myFeat.ftype);
            } else {

				// if it has an init print the name, type, and expression
				// with a flag so that the expression prints its type
                write("attribute_init\n");
                outputID(myFeat.fname);
                outputID(myFeat.ftype);
                output_exp(myFeat.finit, true);
            }
        } else if (myFeat.fmeth == "Method") {

			// if it's a method, print the word method and them it's ID
            write("method\n");
            outputID(myFeat.mname);

            // then print the formals, name and type
            write(myFeat.formals.length + "\n");
            for (var q = 0; q < myFeat.formals.length; q++) {
                outputID(myFeat.formals[q].fname);
                outputID(myFeat.formals[q].ftype);
            }

			// then the return type and the body of the method
			// with the body's expression's type
            outputID(myFeat.mtype);
            output_exp(myFeat.mbody, true);
        } else {
			// sanity check, if it's neither attribute nor method it's impossible
        }
    }
}

//-------------------SECTION 8: Helper Functions

//Create File
function writeFirst(data) {
    fs.writeFileSync(fname, data);
}

//Write to File
function write(data) {
    fs.appendFileSync(fname, data);
}

//Check if 2 arrays (methods in our case) are equal
function arraysEqual(arr1, arr2) {

	// if they don't have the same length they're not equal
    if (arr1.length != arr2.length) {
        return false;
    }
    for (var i = arr1.length; i--;) {
        // looping through the formals
        if (arr1[i].fname.name != arr2[i].fname.name) {
            return false;
        }
        if (arr1[i].ftype.name != arr2[i].ftype.name) {
            return false;
        }
    }
    return true;
}

//check if an item is in a list
function check(item, list) {
    for (var index = 0; index < list.length; index++) {
        if (item == list[index]) {
            return true;
        }
    }
    return false;
}

// get a list of what a class inherits.
// should begin with Object and end with the initally passed in class
function inheritList(classname, listsofar) {
    var indof = user_classes.indexOf(classname);

    listsofar.unshift(classname);
    if (indof == -1) {
        listsofar.unshift("Object");
        return listsofar;
    } else {
        var parent = userClasses[indof].inherit;
        if (parent == "") {
            listsofar.unshift("Object");
            return listsofar;
        } else {
            // console.log(parent.name);
            return inheritList(parent.name, listsofar);
        }
    }
}

// find the join/LUB of two types
function leastCommonAncestor(t1, t2) {
    var l1 = inheritList(t1, []);
    var l2 = inheritList(t2, []);
    // console.log(l1);
    // console.log(l2);

	// get an inheritance list for each
    l1 = l1.reverse();
    l2 = l2.reverse();

	// double for-loop step through them until something matches
	// the last object tried will be Object
	// which is a parent to everything
    for (var i = 0; i < l1.length; i++) {
        for (var j = 0; j < l2.length; j++) {
            if (l1[i] == l2[j]) {
              // console.log(l1[i]);
                return l1[i];
            }
        }
    }
}

// returns true of the child is a valid subclass of parent
function checkInherit(parent, child) {
    // if they're the same, it's ok
	if (parent == child) {
        return true;
    }

	// if the child is a user class
    if (user_classes.indexOf(child) != -1) {
        var childsparent = userClasses[user_classes.indexOf(child)].inherit;

		// check to see if it inherits
		// if it does inherit, call again
		// if it doesn't inherit simply return
        if (childsparent == "") {
			// noinherit, therefore inherits Object
            return parent == "Object" || parent == child;

        } else if (parent == childsparent.name) {
            // it's inherit is the parent
			return true;
        } else {
            // it has some other inherit
			return checkInherit(parent, childsparent.name);
        }

    } else {
        // the child is a base classs
        return parent == "Object" || parent == child;
    }
}
