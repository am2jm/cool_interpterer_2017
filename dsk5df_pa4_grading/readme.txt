-----------TESTING----------------------------------
An automated script we wrote ran all our script

pa4t:
We were able to follow Weimer's python program (from video) to randomly generate ALL 15 testcases

pa4c: For positive testcases we used the hard files and some testcases written in PA3. For the negative testcases, we wrote a .cl file for every type of error

pa4: We were able to combine the testcases written for previous parts of this assignment to test the remaining part of this assignment

good.cl: This is a test file that has several inherited classes, one which uses static dispatch. This uncovered a bug in our code where we could not handle SELF_TYPE when a function was called with SELF_TYPE as an argument. We orginally thought this wasn't possible because SELF_TYPE cannot be declared as a formal parameter. This helped us pass the dispatch-5.cl and newselftype.cl likely because SELF_TYPE was being passed as an argument. In addition it also checks arithmetic, comparisons, cases (and uses of self in case), and dispatch

bad1.cl: This is a test case that uses an identifier out its scope. We realized that when we pushed a variable on the symbol table for let and then removed it, the symbol table for the identifier would point to an empty array. Therefore, for the next access, the typechecking wouldn't catch it because that identifier was still in the symbol table (although it was pointing to nothing)

bad2.cl: This is a bad testcase that has an inheritance cycle. While we were able to handle long inheritance cycles after we finished out topological sort, we actually had more trouble with shorter ones. Becuase A inherits B and B inherits A, the set of starting nodes would be empty, which in our toposort was a condition that indicated a successful toposort. We then fixed the toposort to consider an empty set of starting nodes.

bad3.cl: This is a bad testcase that seemed simple but was one of the last bugs we found. When there is a typechecking failure in an if statment or a while loop, the error is thrown not where the typechecking failed but where the if statement was found.

-------Getting Started----------

So we started by following Weimer's video guide and transcribing his OCaml to Javascript as best we could, we did not notice that Kevin's "JavaScript Guide" was actually Weimer's video guide, but in nodejs. Like Weimer, we structured our code such that in the beginning there are a number of recursive functions that are called to read in the file. We did ours differently in that we load in the entire file as an array, and our read function is actually just incrementing through the file, so that our code can match as closely as possible to the way Weimer has Ocaml read in the ast file. We watched all Weimer videos for each part and regularly attended OH

-------Code Structure----------

Process the AST and Do Basic Negative Checking

  1.) After the entire thing is read in it is stored as an array of Classes that have features
  2.) we create another two arrays, one of base classes: IO, Object, String, Int, Bool, and one to store the names of the user classes for easier access.
    -At this stage we run a number of initial checks, such as:
      -is there a Main class?
      -Do any user classes duplicate the base classes?
  3.) Then we proceed to make a graph of all of the inheritance and run that through a topological sort to determine if there are any inheritance cycles, and if any of the user classes inherit non-inheritable classes.
  4.) After all of these checks for negative behavior we add all of the methods and attributes to specific arrays stored inside the class, rather than stored together as an array of features. We do this in the order returned by the topological sort, thus if a class inherits something we are able to easily grab all of the attributes and methods of the parent and check to ensure the child does not redefine anything in illegal ways.

Using Class objects and Typechecking Them

 The typechecking is very similar to how the program finally outputted, one function calls each class to call each feature and each feature calls its expressions to typecheck:

 1.) we create our object and method symbol tables by mapping each classname to the methods or attributes associated with it. Our expressions have a field "rettype" that is initialized as nothing, but is filled in, inside out as the expressions are all typechecked.

 2.) Each function passes on the current classname, object symbol table, and method symbol table onto each recursive typecheck call so that expressions like let can be typechecked with the correct variables in scope.

We followed each typechecking rule as closely as we could to ensure that our program works correctly and wrapped all the typechecking in a case statement. Once we figured out how to use the object and method symbol tables (and the fact that we even needed one) the rest was mostly following the rules as stated. Each expression at the end of typechecking it gets it's returntype feild set correctly, and because javascript is pass by reference this allows the caller output-expression to correctly typecheck when it returns from it's recursive call.

Handling Base Classes and SELF_TYPE:

  Int, String, and Bool are simply set to their respective classes, but everything else gets the name of it's type. We decided to handle SELF_TYPE by appending the "sub_C" part of the string SELF_TYPE and check the inital substring wherever we check for self-type. This way we can learn if it's a self-type and keep track of the "sub_C" at the same time. (ie: SELF_TYPE|Cons) We simply hardcoded knowing that if there is a sub_C type then it will be all the characters of the return type after the inital 9. We also created two helper functions specifically for typechecking. One is a checkInherit that will accept two Class names and return true or false that the child is a valid subtype of the parent. We also created a join function that will use checkInherit in order to return the types required by "If" and "Case" statements. The join function simply takes two classes and finds their LUB, so the if function simply uses this, and the case function loops all of it's branches through this function to find the LUB to return.

Creating -type file:

  Finally after all the typechecking is complete we have sections that print the class-map, the implementation, the parent map, and the annotated AST in that order. The class map iterated through each class and all of their attributes recursively calling print methods. The implementation map is quite similar except that it goes through the methods rather than the attributes, but likewise recursively calls print methods for all of the expressions, with a for loop running through the many formals. The parent map was quite simple and was just a few loops. Lastly, the newly annotated AST simply runs through the classes in the order they were originally read in by iterating over the original array before classes were sorted, and calling print methods for each piece that needed to be printed. It calls the output expression function with a flag of true, to indicate that said method should print the type of the expression, now that each part including the class-map needs to print the expression's type.
