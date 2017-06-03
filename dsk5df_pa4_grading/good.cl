

class D inherits C{
	x : Object <- (new C)@B.value(new SELF_TYPE);

};

class Main inherits IO {
x : Object <-
case self of
 a : Object => 123;
 b : Int => 345;
 c : String => 456;
esac;


	var : Object <- 345;
	w : Int <- ~44;
	y : Int <- 66-65;
	z : Int <- 88*98;
	a : String <- "divya";
	main() : SELF_TYPE { self };
	g
	 : Object <- (new A).value(new A);

};

class A {
	value(x : A) : SELF_TYPE { self};
};

class B inherits A {


};

class C inherits B{


};
