Simple Method Dispatch
======================

Smalltalk Instances are tagged Scheme Vectors (Arrays of Objects) with slot 0 
containing a reference to the instance Behavior, a hash-table (Identity Dictionary) 
of Selector (a Symbol) -> BlockClosure.

Each Class keeps a Set (list) of Selectors defined in that Class.  When a method is
defined, a new selector is added as well as the binding of selector->closure.  
The Class maintains the Behavior (method dictionary) which is shared with all
its instances.

Defining a method also causes the selector->closure to be added, recursively, to
all child classes -- if the child class does not define an override ["Copy Down"].

Basic shape:
````Smalltalk
	BuddyClass>>introduce: aJoe to: aJane
	   <method definition>	
-->
	#introduce:to -> [ :self :aJoe :aJane | <method definition> ]
````

An instance's class is known by invoking the method #class. 

Method invocation is dirt simple
````Smalltalk
	((self primAt: 0) at: selector) value: self  [value: arg]...
````

If selector lookup fails, a #doesNotUnderstand method of suitable arity is returned,
so a closure is allways invoked.

Assuming efficent hash-tables, this strategy is simple to explain, and eliminates recusrive lookups. 
Lookup can be done without call-site caching.

