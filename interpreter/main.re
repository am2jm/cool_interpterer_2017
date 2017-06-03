/* In order to be able to use Printf */
open Printf;

/**************************************************
SETUP TYPES
**************************************************/

/* Setup the types that we plan to use later
	Mostly all cool item
	We added "Action" as a special type for Let and Case
	to make interperting easier later
*/
type cool_program = list cool_class
and loc = string
and id = (loc, string)
and cool_type = id
and cool_class = (id, option id, list feature)
and feature =
  | Attribute id cool_type (option exp)
  | Method id (list formal) cool_type exp
and formal = (id, cool_type)
and action = (id, id, (option exp))
and exp =
  | Integer string Int32.t
  | String string string
  | Bool string bool
  | Internal string string
  | New string string
  | Dispatch string exp string (list exp)
  | StaticDispatch string exp string string (list exp)
  | Variable string string
  | Assign string string exp
  | Plus string exp exp
  | Minus string exp exp
  | Times string exp exp
  | Divide string exp exp
  | Negate string exp
  | Not string exp
  | Comp string exp exp string
  | Isvoid string exp
  | Case string exp (list action)
  | If string exp exp exp
  | Block string (list exp)
  | While string exp exp
  | Let string (list action) exp
  | Nothing string string
  ;

type cool_address = int;

/* Setup the five basic cool types */
type cool_value =
  | Cool_Int Int32.t
  | Cool_Bool bool
  | Cool_String string
  | Cool_Object string (list (string, cool_address))
  | Void;

/* Setup the env, store, and how we plan to make newlocations later */
type enviroment = list (string, cool_address);
type store = list (cool_address, cool_value);
let new_location_counter = ref 1000;
let newloc () => {
  incr new_location_counter;
  !new_location_counter
};


/* Setup the implementation map and the class map types */
type class_map = list (string, list (string, string, exp));
type imp_map =
  list ((string, string), (list string, string, exp));







/**************************************************
READING INPUT
**************************************************/

/* Setup reading in from the argument which is the -type file */
let fname = Sys.argv.(1);
let fin = open_in fname;

/* define a few functions
	Read simple reads one line
*/
let read () => input_line fin;
let rec range k =>
  if (k <= 0) {
    []
  } else {
    [k, ...range (k - 1)]
  };

 /* Make a readlist function to make reading the various
 	types of lists much easier later
 */
let read_list worker => {
  let k = int_of_string (read ());
  let lst = range k;
  List.map (fun _ => worker ()) lst
};


/* Finally begin reading in the cool program */
let rec read_cool_program () => read_list read_cool_class

/* Setup reading in an identifer, which is a location and a name */
and read_id () => {
  let loc = read ();
  let name = read ();
  (loc, name)
}

/* Defining more things that will be read in later */
and read_cool_class () => {
  let cname = read_id ();
  let inherits =
    switch (read ()) {
    | "no_inherits" => None
    | "inherits" =>
      let super = read_id ();
      Some super
    };
  let features = read_list read_features;
  (cname, inherits, features)
}

/* Setup special functions for class map, implemenation map
	and the parent map
	Otherwise reading the AST is practically the same as always
*/
and read_class_map() => {
read_list read_class_cm
}

and read_class_cm() => {
  let classname = read();
  (classname, read_list read_attrib)
  }

/* reading in the attributes */
and read_attrib() => {
  let init = read();
  let name = read();
  let atype = read();

  let exp = switch (init){
  |"initializer" => Some (read_exp())
  |"no_initializer" => None


  };
  (name, atype, exp)
}

/* Read in the parent map so we can use it to do Case */
and read_parent_map() => {
  read_list parent_mapping
}
and parent_mapping() => {
  let child = read();
  let parent = read();
  (child, parent)
}

/* Read in the implemenation map */
and read_imp_map() => {
  let imp_list = read_list read_class_im;
  List.concat imp_list
}

and read_class_im() => {
    let classname = read();


    let methodlist = read_list read_methods;

    let givename = fun (t): ((string, string), (list string, string, exp)) => {
    let (mname, formals, pclass, mbody) = t;
      ((classname, mname), (formals, pclass, mbody))
    };

    List.map givename methodlist
}

/* Read methods as called by the implementation map
	because they are different than the formals in the AST
*/
and read_methods () => {
  let mname = read();
  let formals = read_list read_formal_im;
  let pclass = read();
  let mbody = read_exp();


  (mname, formals, pclass, mbody)
}


/* Read all the festures as listed in the AST into the AST basically */
and read_features () =>
  {
      switch (read ()) {
      | "attribute_no_init" =>
        let fname = read_id ();
        let ftype = read_id ();
        Attribute fname ftype None
      | "attribute_init" =>
        let fname = read_id ();
        let ftype = read_id ();
        let finit = read_exp ();
        Attribute fname ftype (Some finit)
      | "method" =>
        let mname = read_id ();
        let formals = read_list read_formal;
        let mtype = read_id ();
        let mbody = read_exp ();
        Method mname formals mtype mbody
      | x => failwith ("cannot happen: " ^ x)
      }
  }

 /* Function to read in formals for the AST */
and read_formal () => {
  let fname = read_id ();
  let ftype = read_id ();

  (fname, ftype)
}

/* Reading in a formal in the imp map is slightly different than reading in one in the AST */
and read_formal_im () => {
  let myid = read();
  myid
}

/* read in a type we created for ease of being able to reuse types */
and read_action () => {
    let lettype = read();
    let lid = read_id();
    let mtype = read_id();

    let my_exp = switch(lettype) {
    | "let_binding_init" => (Some (read_exp()))
    | "let_binding_no_init" => None
    | x => failwith("impossible let")
    };
    (lid, mtype, my_exp)

}

/* Read in a case's action, this is separate from Let because
	in a let the expression is optional
*/
and read_action_case () => {
    let case_id = read_id();
    let case_type = read_id();
    let case_exp = read_exp();

    (case_id, case_type, Some (case_exp))


}

/* Reading in the expressions, we create objects of said type that are stored inside
	of whatever is calling this function.
	Each expression has its own pieces as well as the location of where it is in the cool program
	so that the error-exception locations can be printed.

	"self" is simply another variable, and is treated as such
	"self_dispatch" is the same as dynamic except with "self" as the expression that dispatch is called upon
*/
and read_exp () => {

  let eloc = read ();
  let exp_type = read();


  let ekind =
    switch (read ()) {
    | "integer" =>
      let ival = read ();
      Integer eloc (Int32.of_int (int_of_string ival))

    | "string" =>
      let ival = read();
      String eloc ival

    | "assign" =>
      let loc = read();
      let name = read();
      let exp = read_exp();
      Assign eloc name exp

    | "case" =>
      let e0 = read_exp();
      let actions = read_list read_action_case;
      Case eloc e0 actions

    | "internal" =>
      let ival = read();
      Internal eloc ival

    | "identifier" =>
      let loc = read();
      let name = read();
      switch(name){
      | "self" => Variable eloc "self"
      | _ => Variable eloc name
      }

    | "if" =>
      let e1 = read_exp();
      let e2 = read_exp();
      let e3 = read_exp();
      If eloc e1 e2 e3

    | "while" =>
      let e1 = read_exp();
      let e2 = read_exp();
	  While eloc e1 e2

    | "isvoid" =>
        let e1 = read_exp();
        Isvoid eloc e1
    | "block" =>
        Block eloc (read_list read_exp)

    | "plus" =>
      let e1 = read_exp();
      let e2 = read_exp();
      Plus eloc e1 e2

    | "minus" =>
      let e1 = read_exp();
      let e2 = read_exp();
      Minus eloc e1 e2

    | "times" =>
      let e1 = read_exp();
      let e2 = read_exp();
      Times eloc e1 e2

    | "divide" =>
      let e1 = read_exp();
      let e2 = read_exp();
      Divide eloc e1 e2

    | "not" =>
        let e1 = read_exp();
        Not eloc e1

    | "negate" =>
        let e1 = read_exp();
        Negate eloc e1

    | "true" =>
        Bool eloc ( bool_of_string "true")

    | "false" =>
        Bool eloc ( bool_of_string "false")

    | "eq" =>
        let e1 = read_exp();
        let e2 = read_exp();
        Comp eloc e1 e2 "eq"

    | "lt" =>
        let e1 = read_exp();
        let e2 = read_exp();
        Comp eloc e1 e2 "lt"

    | "le" =>
        let e1 = read_exp();
        let e2 = read_exp();
        Comp eloc e1 e2 "le"

    | "new" =>
        let loc = read();
        let name = read();
        New eloc name

    | "dynamic_dispatch" =>
        let exp = read_exp();
        let idloc = read();
        let idname = read();
        let arg = read_list read_exp;
        (Dispatch eloc exp idname arg)

    | "self_dispatch" =>
        let exp = Variable eloc "self";
        let idloc = read();
        let idname = read();
        let arg = read_list read_exp;
        (Dispatch eloc exp idname arg)

    | "static_dispatch" =>
        let exp = read_exp();
        let typeloc = read();
        let typeid = read();
        let idloc = read();
        let idname = read();
        let arg = read_list read_exp;
        (StaticDispatch eloc exp typeid idname arg)

    | "let" =>
        let action_list = read_list read_action;
        let exp = read_exp();
        (Let eloc action_list exp)
    | x => failwith ("do more: " ^ x)
    };
    ekind
};




/* Here is where we are finally calling all of the reads into variables where they will be stored
	We are throwing aways the lines that are simply
	class_map, implemenation_map, parent_map
*/

let x = read();
let class_map = read_class_map();
let y = read();
let imp_map = read_imp_map();
let z = read();
let parent_map = read_parent_map();
let ast = read_cool_program();

close_in fin;



/**************************************************
EVALUTATION - DEBUGGING + COUNTERS
**************************************************/

/* Weimer's debugging functions, very helpful at the start of this project */
let do_debug = ref false;

let debug fmt => {
  let handle result_string =>
    if !do_debug {
      Printf.printf "%s" result_string
    };
  Printf.kprintf handle fmt
};

/* Function that decided how each expression should be printed
	After this is how the enviroment, store, and how values will be printed
	Debugging print statements
*/
let rec exp_to_str e =>
  switch e {
  | New _ s => Printf.sprintf "New(%s)" s
  | Dispatch _ ro fname args [@implicit_arity] =>
    let arg_str = List.fold_left (fun acc elt => acc ^ ", " ^ exp_to_str elt) "" args;
    Printf.sprintf "Dispatch(%s, %s, [%s])" (exp_to_str ro) fname arg_str
  | StaticDispatch _ ro typename fname args [@implicit_arity] =>
    let arg_str = List.fold_left (fun acc elt => acc ^ ", " ^ exp_to_str elt) "" args;
    Printf.sprintf "Dispatch(%s, %s,%s, [%s])" (exp_to_str ro) typename fname arg_str
  | Variable _ x => Printf.sprintf "Variable(%s)" x
  | Assign _ x e [@implicit_arity] => Printf.sprintf "Assign(%s, %s)" x (exp_to_str e)
  | Case _ e0 actions => Printf.sprintf "%s" (exp_to_str e0)
  | Integer _ i => Printf.sprintf "Integer(%ld)" i
  | Plus _ e1 e2 [@implicit_arity] =>
    Printf.sprintf "Plus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | If _ e1 e2 e3 [@implicit_arity] =>
    Printf.sprintf "Plus(%s, %s, %s)" (exp_to_str e1) (exp_to_str e2) (exp_to_str e3)
  | While _ e1 e2 [@implicit_arity] =>
    Printf.sprintf "Plus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Minus _ e1 e2 [@implicit_arity] =>
    Printf.sprintf "Minus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Times _ e1 e2 [@implicit_arity] =>
    Printf.sprintf "Times(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Divide _ e1 e2 [@implicit_arity] =>
    Printf.sprintf "Divide(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | String _ s =>  Printf.sprintf "String(%s)" s
  | Bool _ b => Printf.sprintf "Bool(%b)" b
  | Not _ e1 => Printf.sprintf "Not(%s)" (exp_to_str e1)
  | Negate _ e1 => Printf.sprintf "Negate(%s)" (exp_to_str e1)
  | Block _ e1 =>
     let f = fun(acc: string) (e : exp) => {
      (exp_to_str e) ^ acc
      };
      Printf.sprintf "%s" (List.fold_left f "" e1)
  | Isvoid _ e1 => Printf.sprintf "Isvoid(%s)" (exp_to_str e1)
  | Comp _ e1 e2 s => Printf.sprintf "Comp(%s, %s, %s)" (exp_to_str e1) s (exp_to_str e2)
  | Internal _ name => Printf.sprintf "Internal %s" name
  | Let _ action_list exp => Printf.sprintf "Let %s" (exp_to_str exp)
  | x => failwith ("cannot happen: " ^ "can't print")
  };

let value_to_str v =>
  switch v {
  | Cool_Int i => Printf.sprintf "Int(%ld)" i
  | Cool_Bool b => Printf.sprintf "Bool(%b)" b
  | Cool_String s => Printf.sprintf "String(%s)" s
  | Void => Printf.sprintf "Void"
  | Cool_Object cname attrs [@implicit_arity] =>
    let attr_str =
      List.fold_left
        (fun acc (aname, aaddr) => Printf.sprintf "%s, %s=%d" acc aname aaddr) "" attrs;
    Printf.sprintf "%s([%s])" cname attr_str
  | _ => failwith ("fail in value-to-str " ^ "value_to_str")
  };

let env_to_str env => {
  let binding_str =
    List.fold_left
      (fun acc (aname, aaddr) => Printf.sprintf "%s, %s=%d" acc aname aaddr)
      ""
      (List.sort compare env);
  Printf.sprintf "[%s]" binding_str
};

let store_to_str env => {
  let binding_str =
    List.fold_left
      (
        fun acc (addr, cvalue) =>
          Printf.sprintf "%s, %d=%s" acc addr (value_to_str cvalue)
      )
      ""
      (List.sort compare env);
  Printf.sprintf "[%s]" binding_str
};


/* Counters:
	For debugging, how many indents there should be
	For StackOverflow, keeping track of when the stack should overflow
*/
let indent_count = ref 0;
let overflow_count = ref 0;

/* Function to increment the stackoverflow counter
	and a function to decrement it
*/
let increment_stack (loc) => {
    overflow_count := !overflow_count + 1;
    if(!overflow_count >= 1000){
        Printf.printf "ERROR: %s: Exception: stack overflow\n" loc;
        exit 0;
    }
};
let decrement_stack () => {
    overflow_count := !overflow_count - 1;
};

let debug_indent () => debug "%s" (String.make !indent_count ' ');

/**************************************************
EVALUTATION
**************************************************/

let rec eval (so: cool_value) (s: store) (e: enviroment) (exp: exp) :(cool_value, store) => {
  indent_count := !indent_count + 2;
  debug_indent ();
  debug "\neval: %s\n" (exp_to_str exp);
  debug_indent ();
  debug "self= %s\n" (value_to_str so);
  debug_indent ();
  debug "sto = %s\n" (store_to_str s);
  debug_indent ();
  debug "env = %s\n" (env_to_str e);

  /* End of debug statements
		Begin the actual evaluation by switching on the expression type
  */
  let (new_value, new_store) =
    switch exp {
    | Integer loc i => (Cool_Int i, s)
    | String loc str => (Cool_String str, s)
    | Bool loc b => (Cool_Bool b, s)
    | Not loc e1 =>
        let (v1, s2) = eval so s e e1;
        let result_value =
          switch (v1) {
          | (Cool_Bool b) => Cool_Bool (Pervasives.not b)
          | _ => failwith "impossible in plus"
          };
        (result_value, s2)

    | If loc e1 e2 e3 =>
        let (v1, s2) = eval so s e e1;
        switch(v1){
        | (Cool_Bool true) =>
            let (v2, s3) = eval so s2 e e2;
            (v2, s3)
        | (Cool_Bool false) => let (v2, s3) = eval so s2 e e3;
            (v2, s3)
        }

    | While loc e1 e2 =>
        let (v1, s2) = eval so s e e1;
        switch(v1){
        | (Cool_Bool true) => let (v2, s3) = eval so s e e2;
        eval so s3 e exp
        | (Cool_Bool false) =>
        (Void, s2)
        | _ => failwith "impossible while"
        }

    | Negate loc e1 =>
        let (v1, s2) = eval so s e e1;
        let result_value =
          switch (v1) {
          | (Cool_Int i) => Cool_Int (Int32.neg i)
          | _ => failwith "impossible in plus"
          };
        (result_value, s2 )

    | Isvoid loc e1 =>
        let (v1, s2) = eval so s e e1;
        switch(v1){
            | Void => (Cool_Bool true, s2)
            | _ => (Cool_Bool false, s2)
        }

    | Block loc e1 =>
        let f = fun((cv : cool_value, st :store)) (current_exp : exp) => {
         let (v1, s2) = eval so st e current_exp;
         (v1, s2)
         };

         let(vfinal,sfinal) = List.fold_left f (Void,s) e1;
        /* Printf.printf "\n vfinal and sfinal %s %s: " (value_to_str vfinal) (store_to_str sfinal);*/
         (vfinal, sfinal)


    | Plus loc e1 e2 [@implicit_arity] =>
      let (v1, s2) = eval so s e e1;
      let (v2, s3) = eval so s2 e e2;
      let result_value =
        switch (v1, v2) {
        | (Cool_Int i1, Cool_Int i2) => Cool_Int (Int32.add i1 i2)
        | _ => failwith "impossible in plus"
        };
      (result_value, s3)

    | Minus loc e1 e2 [@implicit_arity] =>
       let (v1, s2) = eval so s e e1;
       let (v2, s3) = eval so s2 e e2;
       let result_value =
         switch (v1, v2) {
         | (Cool_Int i1, Cool_Int i2) => Cool_Int (Int32.sub i1 i2)
         | _ => failwith "impossible in plus"
         };
       (result_value, s3)

    | Times loc e1 e2 [@implicit_arity] =>
        let (v1, s2) = eval so s e e1;
        let (v2, s3) = eval so s2 e e2;
        let result_value =
          switch (v1, v2) {
          | (Cool_Int i1, Cool_Int i2) => Cool_Int (Int32.mul i1 i2)
          | _ => failwith "impossible in plus"
          };
        (result_value, s3)

    | Divide loc e1 e2 [@implicit_arity] =>
        let (v1, s2) = eval so s e e1;
        let (v2, s3) = eval so s2 e e2;
        switch(v2) {
            | Cool_Int num =>
                if (Int32.to_int(num)==0) {
				/* Check to see if we are going to divide by zero */
                    Printf.printf "ERROR: %s: Exception: division by zero\n" loc;
                    exit 0;
                }
        };
        let result_value =
          switch (v1, v2) {
          | (Cool_Int i1, Cool_Int i2) => Cool_Int (Int32.div i1 i2)
          | _ => failwith "impossible in plus"
          };
        (result_value, s3)

    | Comp loc e1 e2 str =>
        let (v1, s2) = eval so s e e1;
        let (v2, s3) = eval so s2 e e2;
        let result_value =
          switch (str) {
          | "eq" => switch(v1, v2){
                    | (Cool_Int i1, Cool_Int i2) => Cool_Bool ( i1 == i2)
                    | (Cool_Bool b1, Cool_Bool b2) => Cool_Bool (b1 == b2)
                    | (Cool_String str1, Cool_String str2) => Cool_Bool ( str1 ==  str2)
                    | _ => Cool_Bool (v1 === v2)
            }
          | "lt" => switch(v1, v2){
                    | (Cool_Int i1, Cool_Int i2) => Cool_Bool ( i1 < i2)
                    | (Cool_Bool b1, Cool_Bool b2) => Cool_Bool (b1 < b2)
                    | (Cool_String str1, Cool_String str2) => Cool_Bool ( str1 <  str2)
                    | _ => Cool_Bool (false)
                    }
          | "le" => switch(v1, v2){
                    | (Cool_Int i1, Cool_Int i2) => Cool_Bool ( i1 <= i2)
                    | (Cool_Bool b1, Cool_Bool b2) => Cool_Bool (b1 <= b2)
                    | (Cool_String str1, Cool_String str2) => Cool_Bool ( str1 <=  str2)
                    | _ => Cool_Bool(v1===v2)
                    }
          | _ => failwith "impossible in compaison"
          };
        (result_value, s3)

    | Assign loc vname rhs [@implicit_arity] =>
      let (v1, s2) = eval so s e rhs;
      let l1 = List.assoc vname e;
      let s3 = [(l1, v1), ...List.remove_assoc l1 s2];

      (v1, s3)

    | New loc cname =>
        increment_stack(loc);

	 /* check if new is called on basic classes */
	  if(cname =="Bool" || cname == "String" || cname =="Int"){
        switch(cname){
        | "Bool" => decrement_stack();
            (Cool_Bool false, s)
        | "String" => decrement_stack();
            (Cool_String "", s)
        | "Int" => decrement_stack();
            (Cool_Int 0l, s)
        }
      }
      else{

	  /* find correct attributes if it is self_type */
      let attrs_and_inits= if (cname =="SELF_TYPE"){
         let self_type = switch(so) {
            | Cool_Bool _=> "Bool"
            | Cool_Int _ => "Int"
            | Cool_String _ => "String"
            | Cool_Object x _ => x
            |_ => failwith("self is failing!")
         };
         List.assoc self_type class_map;
      }
      else{
         List.assoc cname class_map;
      };


	  /* put attributes in environment and store */
      let new_attr_locs = List.map (fun (aname, atype,ainit) => newloc ()) attrs_and_inits;
      let attr_names = List.map (fun (aname, atype, ainit) => aname) attrs_and_inits;
      let attr_types = List.map (fun (aname, atype, ainit) => atype) attrs_and_inits;
      let attr_defaults = List.map (fun(aname, atype, ainit) => switch(atype){
        | "Int" => Cool_Int 0l
        | "String" => Cool_String ""
        | "Bool" => Cool_Bool false
        | _ => Void
      }) attrs_and_inits;
      let default_locs = List.combine new_attr_locs attr_defaults;

      let attrs_and_locs = List.combine attr_names new_attr_locs;
      let v1 = Cool_Object cname attrs_and_locs;

	  /* First set everything to a default and then override attributes if init*/

      let s2 = default_locs @ s;
      let f = fun  (accumulated_store) (aname, atype, ainit) => {
        switch(ainit) {
        | Some ainit =>
               let (_, updated_store) =
                 eval
                   v1
                   accumulated_store
                   attrs_and_locs
                   (Assign loc aname ainit);
               updated_store
          | None => accumulated_store
          }
      };

      let final_store = List.fold_left f s2 attrs_and_inits;
      decrement_stack();
      (v1, final_store)
      }
/* end of new */

    | Variable loc vname =>
      if (vname == "self"){
        (so, s)
      }
      else{
        let l = List.assoc vname e;
        let final_value = List.assoc l s;
        (final_value, s)
      }

    | Dispatch loc e0 fname args =>
      let current_store = ref s;
      increment_stack(loc);

	  /* evaluate arguments and update store */
      let arg_values =
        List.map
          (
            fun arg_exp => {
              let (arg_value, new_store) = eval so !current_store e arg_exp;
              current_store := new_store;
              arg_value
            }
          )
          args;

      let (v0, s_n2) = eval so !current_store e e0;


	  /* switching on value of dispatch expression, determin which classmap to search, then return final store, env*/
      switch v0 {
          | Cool_Object x attrs_and_locs  =>

          /*   list ((string, string), (list string, string, exp));*/
            let (formals, rettype, body) = List.assoc (x, fname) imp_map;
            let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
            let new_formal_env = List.combine formals new_arg_locs;
            let store_update = List.combine new_arg_locs arg_values;
            let s_n3 =
              store_update @ s_n2;
            let final_env = new_formal_env @ attrs_and_locs;
            let (v_final, s_final) = eval v0 s_n3 final_env body;
            decrement_stack();
            (v_final, s_final)

          | Cool_String str =>
              let (formals, rettype, body) = List.assoc ("String", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2;
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | Cool_Bool b =>
              let (formals, rettype, body) = List.assoc ("Bool", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2;
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | Cool_Int i =>
              let (formals, rettype, body) = List.assoc ("Int", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2;
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | _ =>
              Printf.printf "ERROR: %s: Exception: dispatch on void\n" loc;
              exit 0;

          }
/* end of dispatch */

	  /*copy and paste of normal dispatch except dispatching on different class*/
    | StaticDispatch loc e0 typename fname args =>
      increment_stack(loc);
      let current_store = ref s;
      let arg_values =
        List.map
          (
            fun arg_exp => {
              let (arg_value, new_store) = eval so !current_store e arg_exp;
              current_store := new_store;
              arg_value
            }
          )
          args;
      let (v0, s_n2) = eval so !current_store e e0;
      switch v0 {
          | Cool_Object x attrs_and_locs  =>
            let (formals, rettype, body) = List.assoc (typename, fname) imp_map;
            let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
            let new_formal_env = List.combine formals new_arg_locs;
            let store_update = List.combine new_arg_locs arg_values;
            let s_n3 =
              store_update @ s_n2;
            let final_env = new_formal_env @ attrs_and_locs;
            let (v_final, s_final) = eval v0 s_n3 final_env body;
            decrement_stack();
            (v_final, s_final)
          | Cool_String str =>
              let (formals, rettype, body) = List.assoc ("String", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2; /* fixme put the formal parameters first so they are visible and shadow the attributes */
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | Cool_Bool b =>
              let (formals, rettype, body) = List.assoc ("Bool", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2;
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | Cool_Int i =>
              let (formals, rettype, body) = List.assoc ("Int", fname) imp_map;
              let new_arg_locs = List.map (fun arg_exp => newloc ()) args;
              let new_formal_env = List.combine formals new_arg_locs;
              let store_update = List.combine new_arg_locs arg_values;
              let s_n3 =
                store_update @ s_n2;
              let final_env = new_formal_env;
              let (v_final, s_final) = eval v0 s_n3 final_env body;
              decrement_stack();
              (v_final, s_final)
          | _ =>
                Printf.printf "ERROR: %s: Exception: dispatch on void\n" loc;
                exit 0;
          }
/* end of static dispatch*/

    | Internal loc fname =>
      switch(fname){
      | "IO.out_string" =>
	  /* looping through and replacing all instances of \n */
          let loc = List.assoc "x" e;
          let v = List.assoc loc s;
		  /* make a character array */
          let explode s => {
              let rec exp i l =>
                if (i < 0) {
                  l
                } else {
                  exp (i - 1) [s.[i], ...l]
                };
              exp (String.length s - 1) []
            };

		  /* then replace with appropriate ascii character*/
          switch(v) {
            | Cool_String s =>
              let ch_list = explode s;
              let (printme, _) = List.fold_left
                (fun (acc, is_slash) (current_char) => {
                    let mychar = String.make 1 current_char;
                    if(is_slash){
                        if(mychar == "n"){
                            (acc ^ "\n", false)
                        }
                        else if(mychar == "t"){
                            (acc ^ "\t", false)
                        }
                        else{
                            (acc ^ "\\" ^ mychar, false)
                        }
                    }
                    else{
                        if(mychar == "\\"){
                            (acc, true)
                        }
                        else{
                            (acc ^ mychar, false)
                        }
                    }
                })
                ("", false) ch_list;
                Printf.printf "%s" printme;
            | _=> failwith("not a string")
          };
          (so, s)
      | "IO.out_int" =>
          let loc = List.assoc "x" e;
          let v = List.assoc loc s;

          switch(v) {
            | Cool_Int s => Printf.printf "%s" (Int32.to_string s);
            | _=> failwith("not an int boi")
          };
          (so, s)
      | "IO.in_int" =>

	  /* first read, trim, make sure it is a valid number using regex and set to 0 otherwise */
          let new_val =

          try (read_line()) {
              | _ => "0"
          };

          let trimmed_string =  (String.trim new_val);
          let r = Str.regexp "-?[0-9]+";
          if( Str.string_match r trimmed_string 0){
              let finalMatch = Str.matched_string trimmed_string;
              let final_val = try ( Int32.of_string finalMatch) {
              | _ => 0l
                };
              (Cool_Int final_val, s)
          }
          else{
              (Cool_Int (Int32.of_int(0)), s)
          }
      | "IO.in_string" =>
          let new_val = read_line();
          let nullchar = Char.chr 0;
          if(String.contains new_val nullchar) {
            (Cool_String "", s)
          }
          else
          {
            (Cool_String new_val, s)
          }

      | "Object.abort" =>
          Printf.printf "abort\n";
          exit 0;
      | "String.length" =>
         let actual_val = switch(so){
         | Cool_String str => str;
         | _ => failwith ("impossible");
         };
         (Cool_Int (Int32.of_int((String.length actual_val))), s)
      | "String.concat" =>
        let loc = List.assoc "s" e;
        let v = List.assoc loc s;
        let new_value = switch(v){
            |Cool_String str => str;
            | _ => failwith("IMPOSSIBRU");
        };
        let actual_val = switch(so){
        | Cool_String str => str;
        | _ => failwith ("impossible");
        };
        (Cool_String (actual_val ^ new_value), s)

      | "String.substr" =>
        let iloc = List.assoc "i" e;
        let i = List.assoc iloc s;
        let lloc = List.assoc "l" e;
        let l = List.assoc lloc s;
        let index = switch(i){
            |Cool_Int num  => Int32.to_int (num);
            | _ => failwith("IMPOSSIBRU");
        };

        let length = switch(l){
            |Cool_Int num  => Int32.to_int (num);
            | _ => failwith("IMPOSSIBRU");
        };

        let actual_value = switch(so){
            |Cool_String str => str;
            | _ => failwith("IMPOSSIBRU");
        };

		/* make sure the substring is within bounds */
        if( (index + length) > (String.length actual_value) || index < 0 || length < 0){
            Printf.printf "ERROR: 0: Exception: case on void\n";
            exit 0;
        }
        else{
            let final_val = (String.sub (actual_value) (index) (length));

        (Cool_String (final_val), s)
        }

      | "Object.type_name" =>
        switch(so){
        | Cool_Int _ => (Cool_String ("Int"), s)
        | Cool_Bool _ => (Cool_String ("Bool"), s)
        | Cool_String _ => (Cool_String ("String"), s)
        | Cool_Object str _ => (Cool_String (str), s)
        }


      | "Object.copy" =>
	  /* make new location for each attribute but copy value over, make new location for copied object as well */
        let (name, orig_args) = switch (so){
        | Cool_Object name args => (name, args)
        };

        let new_loc = newloc();

        let (final_env, final_store) = List.fold_left
          (
          fun (env, new_store) (name, loc) =>
          {
            let l1 = newloc();
            let current_val = List.assoc loc s;
            (env @ [(name, l1)], new_store @ [(l1, current_val)])
          })
          ([], s) orig_args;

        let new_obj = Cool_Object name final_env;
        (new_obj, [(new_loc, new_obj)] @ final_store)

      | x => failwith("switching on: " ^ x)
      }
/* end if Internal */

    | Let loc action_list e1 =>

        let (s_n, e_n) = List.fold_left (
		/* call eval on each action (assign within let)   */
        fun (my_s, my_e) ((_, id_name), (_, type_name), (exp)) =>{
             let (v1, s1) = switch(exp){
             | Some exp => eval so my_s my_e exp
             | None =>
                let value = switch(type_name) {
                  | "Int" => Cool_Int 0l
                  | "String" => Cool_String ""
                  | "Bool" => Cool_Bool false
                  | _ => Void
                  };
                  (value, [(newloc(), value)] @ my_s)
             };

             let l1 = newloc();
             let s2 = [(l1, v1)] @ s1;
             let e2 = [(id_name, l1)] @ my_e ;
             (s2, e2)
             })
            (s, e) action_list;

        eval so s_n e_n e1


    | Case loc e0 actions =>
        let (v0, s2) = eval so s e e0;
        let actual_type = switch(v0){
        | Void =>
            Printf.printf "ERROR: %s: Exception: case on void\n" loc;
            exit 0;
        | Cool_Int _ => "Int"
        | Cool_Bool _ => "Bool"
        | Cool_String _ => "String"
        | Cool_Object x _ => x
        };

		/* find the ancestor order for desired class based on parent map*/
        let rec findAncestors = fun (t) (my_ancestors) : list string => {
            let (parent) = List.assoc (t) parent_map;
            if (parent == "Object") {
                List.append my_ancestors [parent]
            }
            else {
                findAncestors parent (List.append my_ancestors [parent])

            }
        };

		/* finds all options for case*/
        let case_list = List.fold_left
          (fun acc ((_, t_id), (_, type_name), (exp)) => [(type_name, t_id, exp)] @ acc) [] actions;


        /* list of ancestors of x in order*/
        let ancestors = findAncestors actual_type [actual_type];


        let rec helper = fun (element) (my_ancestors) (counter): int => {
            if (my_ancestors == []) {

                Int32.to_int(Int32.max_int)
            }
            else if (element == (List.hd my_ancestors)) {
                counter + 1;
            }
            else{
             helper element (List.tl my_ancestors) (counter+1)
            }
        };

		/* finds closest ancestor */
        let (_, (closestType, fin_id, fin_exp)) = List.fold_left
          (fun (counter, (finalType, f_id, f_exp)) (current_type, t_id, exp)
          =>
          {
          let new_counter = (helper (current_type) (ancestors) (0));
          let max_num = (Int32.to_int(Int32.max_int));
            if (new_counter < counter) {
              ( new_counter, (current_type, t_id, exp))
              }
            else if (new_counter == max_num && counter == max_num){
                (new_counter, ("Error", f_id, f_exp))
                }
            else{
                (counter, (finalType, f_id, f_exp))
            }
          })
          (Int32.to_int(Int32.max_int),(List.hd case_list)) case_list;
          if (closestType == "Error"){
            Printf.printf "ERROR: %s: Exception: case missing branch\n" loc;
            exit 0;
          };
          let l0 = newloc();
          let s3 = [(l0, v0)] @ s2;
          let e2 = [(fin_id, l0)] @ e;
          switch(fin_exp){
          | Some fin_exp => eval so s3 e2 fin_exp
          | None => (so,s)
          }
/* end of case */

    | _ => failwith "unhandled so far"
    };
  debug_indent ();
  debug "ret = %s\n" (value_to_str new_value);
  debug_indent ();
  debug "sto = %s\n" (store_to_str new_store);
  indent_count := !indent_count - 2;
  (new_value, new_store)
};


/**************************************************
RUNNING PROGRAM
**************************************************/


/* Hardcode an expression that is (new Main).main()
	in order to begin the cool program and run it
	Initialized the store and enviroment and so, as empty lists and Void
*/

/* Initializes fields and calls eval */
let main () => {
  let my_exp = Dispatch "0" (New "0" "Main") "main" [];
  debug "my_exp = %s\n" (exp_to_str my_exp);
  let so = Void;
  let store = [];
  let enviroment = [];
  let (final_value, final_store) = eval so store enviroment my_exp;
  debug "result = %s\n" (value_to_str final_value)
};


/* RUN the program! */
main ();
