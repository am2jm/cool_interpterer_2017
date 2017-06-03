Process:

We began with Weimer's video and used that and office hours to get an idea of how PLY worked. After that we worked our way down the list of Cool Syntax. We were pretty much given class, classlist, and some of feature, so we used that as a basis for the rest of everything we did.

After we finished getting formal and everything above it on the Cool Syntax list working we started making our way up the gigantic list of expressions. Before we attempted any feature, we first wrote a small test file first. We continued this way, with a bunch of hardcoding and slightly suspect coding, using Weimer's code, the Cool Syntax Page, PLY documentation, and the previously given .cl examples.

We let each expression be its own 'exp : <things>' and then just had one giant if-case kind of file choosing the right print for each depending on a value we passed in. This worked extremely well and we only had a few logic-hiccups working our way through the different expressions. The hardest for us was definitely let, especially before we realized that the way the Cool Syntax manual listed it seemed very strange in that instead of being thing [otherthing]*, it was pretty much a [thing2]+ where it had been listed funny because of being comma-separated values. However, we handled it like a list and only ran into some problems when we passed incorrect values/indexes into our parser.

After watching Weimer's videos, we found succeeding in the bad cases was pretty simple, since anything that did not match one of our parses was immediately tossed out as bad. Additionally, with the advice of a TA we set up ensuring that each .cl had to have A class, at least. We simply created one "bad.cl" class when we were failing all of the bad cases, and found that our spacing was entirely incorrect, due in part to the difference in concatenating strings with "+" and "," in python. After we fixed this we re-submitted and found that we had passed all of the bad cases.

Lastly, to test we wrote a bash script to automatically run all the tests and diff (after some prodding and assistance from a benevolent TA). 

Design: 
This was a pretty straightforward assignment that was exactly like the cool syntax manual. For some edge cases, such as “+” symbols versus “*” we would make an intermediate list. and then use that to determine if there was at least one item present.

Test Cases: 
good.cl: this file includes just about every feature that is parsed in cool including, but not exclusive to: let bindings, multiple let bindings, numbers, parenthesis, associativity, features, formals and many other basic expressions
bad.cl:  this is a more simple file that has a faulty let binding and has a faulty class 

