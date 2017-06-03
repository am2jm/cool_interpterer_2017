class Main inherits IO{

	x : String <- "hello";
	y : Int <- x.length();
	my_void_io : IO ;

	main() : Object{
		 new Bubba
	};
};
class Abba inherits IO{
	a : Object <- {out_string("Abba"); 5;};
	b : Object <- new Main;
	c : Bool <- a < b;
	d : Object <- {if c then out_string("true") else out_string("false") fi; "who";};
};

class Bubba inherits Abba{
	e : Abba <- new Abba;
};
