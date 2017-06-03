class Cons inherits List {
	xcar : Int;

	isNil() : Bool {false};

	init (hd : Int, t1 : List) : Cons {
		false
	};
};

class Cons2 inherits List {
        xcar : Int;

        isNil() : Bool {false};

        init (hd : Int, t1 : List) : Cons2 {
                false
        };
};

class C {

    method6(num : Int) : A { -- negate
        (let x : Int in
         {
             x <- ~num;
             (new A).set_var(x);
         }
         )
    };

    method5(num : Int) : E {  -- cube
        (let x : Int in
         {
             x <- num * num * num;
             (new E).set_var(x);
         }
         )
    };

};


class Cons2 inherits List {

    xcar : Int <- 5;
    set_var(num : Int) : SELF_TYPE {
    case var of
        a : A => out_string("Class type is now A\n");
        b : B => out_string("Class type is now B\n");
        esac
    };

    method4(num1 : Int, num2 : Int) : D {
        (let a : A2I <- new A2I in
				     {
                         out_string(a.i2a(r));
                         out_string("\n");
                     }
         )
    };
};

class Cons2 inherits List {
    xcar : Int <- (5);
    xdar : String <- "TA Safe String";

    isNil() : Bool {false};

    init (hd : Int, t1 : List) : Cons2 {
        false
    };
};

class Cons2 inherits List {

    xcar : Int <- 5;
    set_var(num : Int) : SELF_TYPE {
    case var of
        a : A => out_string("Class type is now A\n");
        b : B => out_string("Class type is now B\n");
      		esac
   	};

    main() : Object {
        method5(5,6)
    };
    method4(num1 : Int, num2 : Int) : D {  -- diff
        if char = "d" then avar <- (new C)@A.method5(avar.value()) else 5 fi
            };
   	method5(num4 : Int, num5 : Int) : D {  -- diff
        if true then
            "TA safe string"
            else
                i2a_aux(next).concat(i2c(i - next * 10))
                fi
                };

    method6(num6 : Int, num7 : Int) : D {  -- diff
        if (new A).method7(avar.value()) then
            num6 + num7
            else
                num6 - num7
                fi
                };

};
