class A inherits B{

   var : Object <- 0;

   value(x : Object) : Object { x };

   };
class B inherits A {

   avar : Object <- 0;

   value(x : Object) : Object { x };

   };
class Main inherits IO {
	var : Object <- 345;
	x : Object <- (new B)@A.value(var);
	y : Object <- 999;
	a : String <- "divya";
	main() : Object { 777 };
	value(z: Object) : Object { z };
};
