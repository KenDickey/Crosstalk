/* object.h -- UMB Scheme, object interface.

UMB Scheme Interpreter  				$Revision: 3.2 $
Copyright (C) 1988, 1996 William R Campbell

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

UMB Scheme was written by Bill Campbell with help from Karl Berry,
Barbara Dixey, Ira Gerstein, Mary Glaser, Kathy Hargreaves, Bill McCabe,
Long Nguyen, Susan Quina, Jeyashree Sivasubram, Bela Sohoni, John Tam 
and Thang Quoc Tran.

For additional information about UMB Scheme, contact the author:

	Bill Campbell
	Department of Mathematics and Computer Science
	University of Massachusetts at Boston
	Harbor Campus
	Boston, MA 02125

	Telephone: 617-287-6449		Internet: bill@cs.umb.edu

*/

/* Declarations for all the data objects and forms.

The Scheme interpreter operates on the C type `Object', meant to represent 
a Scheme object; thus, Object is our central data abstraction.

The data type, Object, is the union of many sub-types which taken together
can be used to denote the programs we are interpreting.  Most types 
(e.g., Number_Object, String_Object, If_Object) correspond to objects and
special forms that are discussed in the Scheme Reference Manual.  Others
(e.g.,  Frame_Object) are necessary to our implementation.

There are six operations that are generally applicable to all objects:

1.  Eval_Object		-- directs the explicit evaluator for this type.
2.  Compile_Object	-- compiles the result of a (read) to code.
3.  Display_Object	-- in human readable (eg "cat" => cat) form.
4.  Write_Object	-- in machine readable (eg "cat" => "cat") from.
5.  Show_Object		-- as part of the environment (in abbreviated form).
5.  GC_Object	-- for gc-ing objects of this type on the heap.

We can think of a sub-type as being defined by how these operations are
particularly defined over that sub-type.  This is our basis for representing
objects.  An object is represented by (a pointer to) a structure having one
or more components, including (a pointer to) an operation vector of the five
procedures that implement these operations.  An object's type is distinguished
by its operation vector; e.g., Number_Objects are designated by (the pointer to)
the vector of operations for evaluating, compiling, displaying, writing, and
garbage collecting Number_Objects. */


/* Here are the six common operations. */

typedef struct
{
	void (*Eval)();
	void (*Compile)();
	Integer (*Display)();
	Integer (*Write)();
	Integer (*Show)();
	struct Object_Struct *(*GC)();
} Op_Vector;

typedef Op_Vector *Scheme_Type;

/* The value register needs to be assigned a type t quite often. The call
`Set_Result_Type(t)' will do it. */

#define Set_Result_Type(t) { Get_Type(Value_Register) = t;\
			     Get_Type_Name(Value_Register) = ""  /* #t */; }

/* Every object has at least a `Common_Struct'. */
typedef struct
{
	Scheme_Type Type;
	String Type_Name;
} Common_Struct;

/* And a way to access the common fields. */

#define Get_Type(o) ((o)->Common.Type)
#define Get_Type_Name(o) ((o)->Common.Type_Name)

#define Eval_Object(o)		((Get_Type(o)->Eval)(o))
#define Compile_Object(o)	((Get_Type(o)->Compile)())
#define Display_Object(o,m)	((Get_Type(o)->Display)(o,m))
#define Write_Object(o,m)	((Get_Type(o)->Write)(o,m))
#define Show_Object(o,m)	((Get_Type(o)->Show)(o,m))
#define GC_Object(o)		((Get_Type(o)->GC)(o))

/* Boolean; only the objects: The_True_Object and The_False_Object */

typedef struct
{
	int dummy;
} Boolean_Struct;

Import	Op_Vector Boolean_Ops;
Import	Integer	Boolean_Print();
Import	struct	Object_Struct *Boolean_GC();

Import	Scheme_Type Boolean_Type;

Import	struct Object_Struct *The_True_Object, *The_False_Object;

#define Is_Boolean(o) (Get_Type(o) == Boolean_Type)
#define Is_False(o) ((o) == The_False_Object)

#define Boolean_Size (sizeof(Boolean_Struct)+sizeof(Common_Struct))

/* Pair. */

typedef struct
{
	struct Object_Struct *Car;
	struct Object_Struct *Cdr;
} Pair_Struct;

Import	Op_Vector Pair_Ops;
Import	Integer Pair_Display(), Pair_Write(), Pair_Show();
struct	Object_Struct *Pair_GC();

Import	Scheme_Type Pair_Type;

#define Is_Pair(o) (Get_Type(o) == Pair_Type)
#define Is_List(o) (Is_Pair(o) || Is_Empty_List(o))

#define Pair_Size (sizeof(Pair_Struct)+sizeof(Common_Struct))

#define Get_Pair_Car(o) ((o)->Specific.Pair.Car)
#define Get_Pair_Cdr(o) ((o)->Specific.Pair.Cdr)

Import	void Make_Pair();
Import	Integer Length();
Import	struct Object_Struct *First();
Import	struct Object_Struct *Second();
Import	struct Object_Struct *Third();
Import	struct Object_Struct *Fourth();
Import	struct Object_Struct *Rest();
Import	Boolean	Member();

/* Empty List */

typedef struct
{
	int dummy;
} Empty_List_Struct;

Import	Op_Vector Empty_List_Ops;
Import	Integer Empty_List_Print();
Import	Integer Empty_List_Display();
Import	Integer Empty_List_Write();
struct	Object_Struct *Empty_List_GC();

Import	Scheme_Type Empty_List_Type;

Import	struct Object_Struct *Nil;

#define Is_Empty_List(o) (Get_Type(o) == Empty_List_Type)
#define Empty_List_Size (sizeof(Empty_List_Struct)+sizeof(Common_Struct))

/* Symbol. */
typedef struct
{
	struct	Object_Struct *Global_Binding;
	struct	Object_Struct *How;
	struct	Object_Struct *Property_List;
	Boolean	User_Defined;
	String	Name;
} Symbol_Struct;

Import	Op_Vector Symbol_Ops;
Import	Integer	Symbol_Print();
struct	Object_Struct *Symbol_GC();

Import	Scheme_Type Symbol_Type;

Import	struct Object_Struct *QUOTE_Symbol, *DEFINE_Symbol,*SET_Symbol,
*IF_Symbol, *DEFMACRO_Symbol, *BEGIN_Symbol,
*DELAY_Symbol, *LAMBDA_Symbol, *Special_Binding;
Import	struct Object_Struct *The_Syntactic_Keyword, *The_Undefined_Symbol,
				*An_Argument;

#define Is_Symbol(o) (Get_Type(o) == Symbol_Type)
#define Symbol_Size (sizeof(Symbol_Struct)+sizeof(Common_Struct))

#define Get_Global_Binding(o)	((o)->Specific.Symbol.Global_Binding)
#define	Get_Symbol_How(o)	((o)->Specific.Symbol.How)
#define	Get_Symbol_User_Defined(o) ((o)->Specific.Symbol.User_Defined)
#define Get_Property_List(o)	((o)->Specific.Symbol.Property_List)
#define Get_Symbol_Name(o)	((o)->Specific.Symbol.Name)

/* Is_Special_Symbol is not an lvalue. `Special_Binding' is simply
   a dummy object. */

#define Is_Special_Symbol(o) (Get_Global_Binding(o)==Special_Binding)

Import	void Make_Symbol();

/* Number. */

typedef Integer Tower_Position;

#define FIXNUM_LEVEL 	0
#define BIGNUM_LEVEL 	1
#define RATIONAL_LEVEL	2
#define	REAL_LEVEL	3
#define	COMPLEX_LEVEL	4


#define TOWER_LEVEL_COUNT 5

/* Fixnums */

#define Get_Number_Fixnum_Value(n) ((n)->Specific.Number.Specific.Fixnum_Value)


/* Bignums */

typedef Short Number_Digit_Type;

typedef struct
{
	Integer Length;
	Number_Digit_Type Digits[1];
} Bignum_Struct;

#define Get_Number_Length(n) ((n)->Specific.Number.Specific.Bignum.Length)
#define Get_Number_Digits(n) ((n)->Specific.Number.Specific.Bignum.Digits)
#define Get_Number_Digit(n,i) (((n)->Specific.Number.Specific.Bignum.Digits)[i])

/* Rationals */

typedef struct
{
	struct Object_Struct *Numerator;
	struct Object_Struct *Denominator;
} Rational_Struct;

#define Get_Number_Rational_Numerator(n)\
		((n)->Specific.Number.Specific.Rational.Numerator)
#define Get_Number_Rational_Denominator(n)\
		((n)->Specific.Number.Specific.Rational.Denominator)

/* Reals */

#define Get_Number_Real_Value(n) ((n)->Specific.Number.Specific.Real_Value)

/* Complex Numbers */

typedef struct
{
	Double	Real_Part;
	Double	Imaginary_Part;
} Complex_Struct;

#define	Get_Number_Complex_Real_Part(n)\
		((n)->Specific.Number.Specific.Complex.Real_Part)
#define	Get_Number_Complex_Imaginary_Part(n)\
		((n)->Specific.Number.Specific.Complex.Imaginary_Part)

typedef struct
{
	Tower_Position Position;
	Boolean	Is_Exact;

	union
	{
		Short Fixnum_Value;
		Bignum_Struct Bignum;
		Rational_Struct Rational;
		Double Real_Value;
		Complex_Struct Complex;
	} Specific;
} Number_Struct;

Import	Op_Vector Number_Ops;
Import	Integer	Number_Print();
struct	Object_Struct *Number_GC();

Import	Scheme_Type Number_Type;

#define Is_Number(o) (Get_Type(o) == Number_Type)

/* These number classes are unimplemented. Perhaps they should each have 
their own `_Struct's. The `Number_Struct' here wastes a little space.
(Except for the biggest number.) */
#define Fixnum_Size (sizeof(Number_Struct)+sizeof(Common_Struct))
#define Bignum_Size(n) (sizeof(Common_Struct)+sizeof(Tower_Position)+\
			sizeof(Boolean)+\
	              sizeof(Bignum_Struct)+(n-1)*sizeof(Number_Digit_Type))

#define Rational_Size (sizeof(Number_Struct)+sizeof(Common_Struct))
#define Real_Size (sizeof(Number_Struct)+sizeof(Common_Struct))
#define Complex_Size (sizeof(Number_Struct)+sizeof(Common_Struct))

#define Get_Number_Tower_Position(o) ((o)->Specific.Number.Position)
#define Is_Exact_Number(o)	((o)->Specific.Number.Is_Exact)

/* Make_Number doesn't exist, only Make_..._Number. */

Import	void Make_Fixnum_Number();
Import	void Make_Bignum_Number();
Import	void Make_Rational_Number();
Import	void Make_Real_Number();
Import	void Make_Complex_Number();

/* Character. */

typedef struct
{
	Character Value;
} Character_Struct;

Import	Op_Vector Character_Ops;
Import	Integer	Character_Write(), Character_Display();
struct	Object_Struct *Character_GC();

Import	Scheme_Type Character_Type;

#define Is_Character(o) (Get_Type(o) == Character_Type)

#define Character_Size (sizeof(Character_Struct)+sizeof(Common_Struct))

#define Get_Character_Value(o) ((o)->Specific.Character.Value)

Import	void Make_Character();

/* String. 

	This is a variable-length object; the `1' here is merely a 
	placeholder. Note that in this case, `*Value' and `Value[1]' are not
	equivalent. 
*/

typedef struct
{
	Integer Length;
	Character Value[1];
} String_Struct;

Import	Op_Vector String_Ops;
Import	Integer	String_Write(), String_Display();
struct	Object_Struct *String_GC();

Import	Scheme_Type String_Type;

#define Is_String(o) (Get_Type(o) == String_Type)

/* `String_Size' computes the length of a string object for a string of 
size `n'. (`Integer' is the other field in `String_Struct'.) */
#define String_Size(n) ((n-1)+sizeof(String_Struct)+sizeof(Common_Struct))

#define Get_String_Length(o) ((o)->Specific.String.Length)
#define Get_String_Value(o)  (&((o)->Specific.String.Value[0]))

Import	void Make_String(/*Integer*/);
Import	void Make_Constant_String(/*string*/);
Import	String Copy_String(/* string */);

/* Vector. */

typedef struct
{
	Integer Length;
	struct Object_Struct *Value[1];
} Vector_Struct;

Import	Op_Vector Vector_Ops;
Import	Integer	Vector_Display(), Vector_Write(), Vector_Show();
struct	Object_Struct *Vector_GC();

Import	Scheme_Type Vector_Type;

#define Is_Vector(o) (Get_Type(o) == Vector_Type)

/* The extra space for the elements (beyond the first) is allocated
    via a parameter to Make_Vector. */
#define Vector_Size(n) ((n-1)*sizeof(Object)+sizeof(Vector_Struct)\
							+sizeof(Common_Struct))

#define Get_Vector_Length(o) ((o)->Specific.Vector.Length)
#define Get_Vector_Value(o)  ((o)->Specific.Vector.Value)
#define Get_Vector_Elem(o,i) ((o)->Specific.Vector.Value[i])

Import	void Make_Vector();

/* Procedure.  (What lambda evaluates to). */

typedef struct
{
	String	Name;
	Integer Numargs;
	Boolean	Tracing;
	Boolean	Has_Rest;
	struct	Object_Struct *Body; /* A lambda form. */
	struct	Object_Struct *Environment; /* An environment frame. */
	struct	Object_Struct *Frame; /* For the arguments. */
} Procedure_Struct;

Import	Op_Vector Procedure_Ops;
Import	Integer	Procedure_Print(), Procedure_Show();
struct	Object_Struct *Procedure_GC();

Import	Scheme_Type Procedure_Type;

#define Is_Procedure(o) (Get_Type(o) == Procedure_Type)

#define Procedure_Size (sizeof(Procedure_Struct)+sizeof(Common_Struct))

#define Get_Procedure_Name(o) ((o)->Specific.Procedure.Name)
#define Get_Procedure_Numargs(o) ((o)->Specific.Procedure.Numargs)
#define Get_Procedure_Tracing(o) ((o)->Specific.Procedure.Tracing)
#define Get_Procedure_Has_Rest(o) ((o)->Specific.Procedure.Has_Rest)
#define Get_Procedure_Body(o) ((o)->Specific.Procedure.Body)
#define Get_Procedure_Environment(o) ((o)->Specific.Procedure.Environment)
#define Get_Procedure_Frame(o) ((o)->Specific.Procedure.Frame)

Import	void Make_Procedure();

#define Is_Function(o) (Is_Procedure(o) || Is_Continuation(o) || Is_Primitive(o))

/* Primitive. (Scheme procedures implemented in C.) */

typedef struct
{
	String	Name;
	Boolean	Tracing;
	void	(*Procedure)();
	Integer Arg_Count;
	Scheme_Type Arg_Type[1];
} Primitive_Struct;

Import	Op_Vector Primitive_Ops;
Import	Integer	Primitive_Print();
struct	Object_Struct *Primitive_GC();

Import	Scheme_Type Primitive_Type;

#define Is_Primitive(o) (Get_Type(o) == Primitive_Type)

#define	VARYING	(5)

#define Primitive_Size(n) (((n==VARYING)?0:n-1)*sizeof(Scheme_Type)\
				+sizeof(Primitive_Struct)+sizeof(Common_Struct))

#define Get_Primitive_Name(o) ((o)->Specific.Primitive.Name)
#define Get_Primitive_Tracing(o) ((o)->Specific.Primitive.Tracing)
#define Get_Primitive_Procedure(o) ((o)->Specific.Primitive.Procedure)
#define Get_Primitive_Numargs(o) ((o)->Specific.Primitive.Arg_Count)
#define Get_Primitive_Argtypes(o,i) ((o)->Specific.Primitive.Arg_Type[i])

Import	void Make_Primitive();

/* Continuation. */

typedef struct
{
	struct Object_Struct *State; /* A state frame. */
	Integer Stacksize;
	struct Object_Struct *Stack[1];
} Continuation_Struct;

Import	Op_Vector Continuation_Ops;
Import	Integer	Continuation_Print();
struct	Object_Struct *Continuation_GC();

Import	Scheme_Type Continuation_Type;

#define Is_Continuation(o) (Get_Type(o) == Continuation_Type)
#define Continuation_Size(n) ((n-1)*sizeof(Object)+sizeof(Continuation_Struct)+\
			      sizeof(Common_Struct))

#define Get_Continuation_State(o) ((o)->Specific.Continuation.State)
#define Get_Continuation_Stacksize(o) ((o)->Specific.Continuation.Stacksize)
#define Get_Continuation_Stack_Elem(o,i) ((o)->Specific.Continuation.Stack[i])

Import	void Make_Continuation();


/* Port. */

typedef struct
{
	Boolean Is_Input;
	FILE *File;
	String Name;
} Port_Struct;

Import	Op_Vector Port_Ops;
Import	Integer	Port_Print();
struct	Object_Struct *Port_GC();

Import	Scheme_Type Port_Type;

Import	struct Object_Struct *Current_Input_Port,  *Current_Output_Port,  
                             *Current_Error_Port, *The_Transcript_Port;

#define Is_Port(o) (Get_Type(o) == Port_Type)
#define Port_Size (sizeof(Port_Struct)+sizeof(Common_Struct))

#define Is_Input_Port(o) ((o)->Specific.Port.Is_Input)
#define Is_Output_Port(o) (! Is_Input_Port(o))

#define Get_Port_Name(o) ((o)->Specific.Port.Name)
#define Get_Port_File(o) ((o)->Specific.Port.File)

Import	void	Make_Port();


/* EOF (is its own type). */

typedef struct
{
	int dummy;
} Eof_Struct;

Import	Op_Vector Eof_Ops;
Import	Integer	Eof_Write(), Eof_Display();
struct	Object_Struct *Eof_GC();

Import	Scheme_Type Eof_Type;

#define Is_Eof(o) (Get_Type(o) == Eof_Type)
#define Eof_Size (sizeof(Eof_Struct)+sizeof(Common_Struct))

Import	struct Object_Struct *The_Eof_Object;

/* Variable (lexically addressed via frame number and displacement). */

typedef struct
{
	Boolean Local;
	Integer Frame_Number;
	Integer Displacement;
	struct Object_Struct *Variable_Symbol;
} Variable_Struct;

Import	Op_Vector Variable_Ops;
Import	void	Variable_Eval(); 
Import	Integer	Variable_Print();
struct	Object_Struct *Variable_GC();

Import	Scheme_Type Variable_Type;

#define Is_Variable(o) (Get_Type(o) == Variable_Type)
#define Variable_Size  (sizeof(Variable_Struct)+sizeof(Common_Struct))

#define Is_Local_Variable(o) ((o)->Specific.Variable.Local)
#define Get_Variable_Frame_Number(o) ((o)->Specific.Variable.Frame_Number)
#define Get_Variable_Displacement(o) ((o)->Specific.Variable.Displacement)
#define Get_Variable_Symbol(o) ((o)->Specific.Variable.Variable_Symbol)

Import	void Make_Global_Variable();
Import	void Make_Local_Variable();

/* Apply */

typedef struct
{
	struct Object_Struct *Operator;
	Integer Numargs;
	struct Object_Struct *Arg_List;
} Apply_Struct;

Import	Op_Vector Apply_Ops;
Import	void Apply_Eval(); 
Import	Integer	Apply_Print();
struct	Object_Struct *Apply_GC();

Import	Scheme_Type Apply_Type;

#define Is_Apply(o) (Get_Type(o) == Apply_Type)
#define Apply_Size (sizeof(Apply_Struct)+sizeof(Common_Struct))

#define Get_Apply_Operator(o) ((o)->Specific.Apply.Operator)
#define Get_Apply_Numargs(o)  ((o)->Specific.Apply.Numargs)
#define Get_Apply_Arguments(o) ((o)->Specific.Apply.Arg_List)

Import	void Make_Apply();

/* Lambda  (An expression form that evaluates to a procedure). */

typedef struct 
{
	Integer Numargs;
	Boolean Has_Rest;
	struct Object_Struct *Frame; /* An environment frame. */
	struct Object_Struct *Body; /* Anything. */
} Lambda_Struct;

Import	Op_Vector Lambda_Ops;
Import	void Lambda_Eval(); 
Import	Integer	Lambda_Print(), Lambda_Show();
struct	Object_Struct *Lambda_GC();

Import	Scheme_Type Lambda_Type;

#define Is_Lambda(o) (Get_Type(o) == Lambda_Type)
#define Lambda_Size  (sizeof(Lambda_Struct)+sizeof(Common_Struct))

#define Get_Lambda_Numargs(o) ((o)->Specific.Lambda.Numargs)
#define Get_Lambda_Has_Rest(o) ((o)->Specific.Lambda.Has_Rest)
#define Get_Lambda_Frame(o) ((o)->Specific.Lambda.Frame)
#define Get_Lambda_Body(o) ((o)->Specific.Lambda.Body)

Import	void Make_Lambda();

/* Conditional  (if condition consequent alternate) */

typedef struct 
{
	struct Object_Struct *Condition;
	struct Object_Struct *Consequent;
	struct Object_Struct *Alternate;
} Conditional_Struct;

Import	Op_Vector Conditional_Ops;
Import	void Conditional_Eval(); 
Import	Integer	Conditional_Print(), Conditional_Show();
struct	Object_Struct *Conditional_GC();

Import	Scheme_Type Conditional_Type;

#define Is_Conditional(o) (Get_Type(o) == Conditional_Type)
#define Conditional_Size  (sizeof(Conditional_Struct)+sizeof(Common_Struct))

#define Get_Conditional_Test(o) ((o)->Specific.Conditional.Condition)
#define Get_Conditional_Consequent(o) ((o)->Specific.Conditional.Consequent)
#define Get_Conditional_Alternate(o) ((o)->Specific.Conditional.Alternate)

Import	void Make_Conditional();

/* Assignment  (set! lvalue rvalue) */

typedef struct
{
	struct Object_Struct *Lvalue;
	struct Object_Struct *Rvalue;
} Assignment_Struct;

Import	Op_Vector Assignment_Ops;
Import	void Assignment_Eval(); 
Import	Integer	Assignment_Print();
struct	Object_Struct *Assignment_GC();

Import	Scheme_Type Assignment_Type;

#define Is_Assignment(o) (Get_Type(o) == Assignment_Type)
#define Assignment_Size (sizeof(Assignment_Struct)+sizeof(Common_Struct))

#define Get_Assignment_Lvalue(o) ((o)->Specific.Assignment.Lvalue)
#define Get_Assignment_Rvalue(o) ((o)->Specific.Assignment.Rvalue)

Import	void Make_Assignment();

/* Definition  (define lvalue rvalue)  */

typedef struct
{
	struct Object_Struct *Lvalue;
	struct Object_Struct *Rvalue;
} Definition_Struct;

Import	Op_Vector Definition_Ops;
Import	void Definition_Eval(); 
Import	Integer	Definition_Print();
struct	Object_Struct *Definition_GC();

Import	Scheme_Type Definition_Type;

#define Is_Definition(o) (Get_Type(o) == Definition_Type)
#define Definition_Size  (sizeof(Definition_Struct)+sizeof(Common_Struct))

#define Get_Definition_Lvalue(o) ((o)->Specific.Definition.Lvalue)
#define Get_Definition_Rvalue(o)  ((o)->Specific.Definition.Rvalue)

Import	void Make_Definition();

/* Defmacro Definition */

typedef struct
{
	struct Object_Struct *Keyword;
	struct Object_Struct *Transformer;
} Defmacro_Struct;

Import	Op_Vector Defmacro_Ops;
Import	Integer	Defmacro_Print(), Defmacro_Show();
struct	Object_Struct *Defmacro_GC();

Import	Scheme_Type Defmacro_Type;

#define Is_Defmacro(o) (Get_Type(o) == Defmacro_Type)
#define Defmacro_Size  (sizeof(Defmacro_Struct)+sizeof(Common_Struct))

#define Get_Defmacro_Keyword(o) ((o)->Specific.Defmacro.Keyword)
#define Get_Defmacro_Transformer(o) ((o)->Specific.Defmacro.Transformer)

Import	void Make_Defmacro();

/* Defmacro Call */

typedef struct
{
	struct Object_Struct *Original;
	struct Object_Struct *Expansion;
} Defmacro_Call_Struct;

Import	Op_Vector Defmacro_Call_Ops;
Import	void Defmacro_Call_Eval(); 
Import	Integer Defmacro_Call_Print();
struct	Object_Struct *Defmacro_Call_GC();

Import	Scheme_Type Defmacro_Call_Type;

#define Is_Defmacro_Call(o) (Get_Type(o) == Defmacro_Call_Type)
#define Defmacro_Call_Size  (sizeof(Defmacro_Call_Struct)+sizeof(Common_Struct))

#define Get_Defmacro_Call_Original(o) ((o)->Specific.Defmacro_Call.Original)
#define Get_Defmacro_Call_Expansion(o) ((o)->Specific.Defmacro_Call.Expansion)

Import	void Make_Defmacro_Call();

/* Sequence */

typedef struct
{
	struct Object_Struct *Clauses;
	Boolean From_Begin;
} Sequence_Struct;

Import	Op_Vector Sequence_Ops;
Import	void Sequence_Eval(); 
Import	Integer	Sequence_Print();
struct	Object_Struct *Sequence_GC();

Import	Scheme_Type Sequence_Type;

#define Is_Sequence(o) (Get_Type(o) == Sequence_Type)
#define Sequence_Size  (sizeof(Sequence_Struct)+sizeof(Common_Struct))

#define Get_Sequence_Clauses(o) ((o)->Specific.Sequence.Clauses)
#define Get_Sequence_From_Begin(o) ((o)->Specific.Sequence.From_Begin)

Import	void Make_Sequence();


/* Delay  (a form that evalautes to a promise). */

typedef struct
{
	struct Object_Struct *Expression;
} Delay_Struct;

Import	Op_Vector Delay_Ops;
Import	void Delay_Eval(); 
Import	Integer Delay_Print();
struct	Object_Struct *Delay_GC();

Import	Scheme_Type Delay_Type;

#define Is_Delay(o) (Get_Type(o) == Delay_Type)
#define Delay_Size  (sizeof(Delay_Struct)+sizeof(Common_Struct))

#define Get_Delay_Expression(o) ((o)->Specific.Delay.Expression)

Import	void Make_Delay();

/* Promise */

typedef struct
{
	struct Object_Struct *Expression;
	struct Object_Struct *Environment;
	Boolean Forced;
} Promise_Struct;

Import	Op_Vector Promise_Ops;
Import	Integer Promise_Print(), Promise_Show();
struct	Object_Struct *Promise_GC();

Import	Scheme_Type Promise_Type;

#define Is_Promise(o) (Get_Type(o) == Promise_Type)
#define Promise_Size  (sizeof(Promise_Struct)+sizeof(Common_Struct))

#define Get_Promise_Expression(o) ((o)->Specific.Promise.Expression)
#define Get_Promise_Environment(o) ((o)->Specific.Promise.Environment)
#define Get_Promise_Forced(o)   ((o)->Specific.Promise.Forced)

Import	void Make_Promise();

/* Error -- evalauates to an error */

typedef struct
{
	String Message;
} Error_Struct;

Import	Op_Vector Error_Ops;
Import	Integer Error_Print();
struct	Object_Struct *Error_GC();

Import	Scheme_Type Error_Type;

#define Is_Error(o) (Get_Type(o) == Error_Type)
#define Error_Size  (sizeof(Error_Struct)+sizeof(Common_Struct))

#define Get_Error_Message(o) ((o)->Specific.Error.Message)

Import	void Make_Error();

/* Environment Frame */

typedef struct
{
	struct Object_Struct *Previous_Frame; /* Another environment frame. */
	Integer Size;
	Boolean Has_Rest;
	struct Object_Struct *Slots[3];
} Environment_Frame_Struct;

Import	Op_Vector Environment_Frame_Ops;
Import	void	Environment_Frame_Eval(); 
Import	Integer	Environment_Frame_Print(), Environment_Frame_Show();
struct	Object_Struct *Environment_Frame_GC();

Import	Scheme_Type Environment_Frame_Type;

Import	struct Object_Struct *The_Global_Environment;

#define Is_Environment_Frame(o) (Get_Type(o) == Environment_Frame_Type)
#define Environment_Frame_Size(n)  ((n-1)*3*sizeof(Object)+\
				     sizeof(Environment_Frame_Struct)+\
                                     sizeof(Common_Struct))

#define Get_Environment_Frame_Previous(o) \
			((o)->Specific.Environment_Frame.Previous_Frame)
#define Get_Environment_Frame_Size(o) ((o)->Specific.Environment_Frame.Size)
#define Get_Environment_Frame_Has_Rest(o) ((o)->Specific.Environment_Frame.\
						Has_Rest)

#define Get_Environment_Frame_Binding_Symbol(o,i) \
      ((o)->Specific.Environment_Frame.Slots[i])
#define Get_Environment_Frame_Binding_Value(o,i) \
      ((o)->Specific.Environment_Frame.Slots[Get_Environment_Frame_Size(o)+i])
#define Get_Environment_Frame_Binding_How(o,i) \
      ((o)->Specific.Environment_Frame.Slots[2*Get_Environment_Frame_Size(o)+i])

Import	void Make_Symbol_Frame();
Import	void Make_Environment_Frame();


/* State Frame. */

typedef struct
{
	struct	Object_Struct *Expression;
	struct	Object_Struct *Environment;
	struct	Object_Struct *Function;
	struct	Object_Struct *Arg_List;
	struct	Object_Struct *State;
	ELabel	PC;
	Integer	Stack_Top_Ptr;
} State_Frame_Struct;

Import	Op_Vector State_Frame_Ops;
Import	void State_Frame_Eval();
Import	Integer	State_Frame_Print();
struct	Object_Struct *State_Frame_GC();

Import	Scheme_Type State_Frame_Type;

#define Is_State_Frame(o) (Get_Type(o) == State_Frame_Type)
#define State_Frame_Size  (sizeof(State_Frame_Struct)+sizeof(Common_Struct))

#define Get_State_Frame_Expression(o) ((o)->Specific.State_Frame.Expression)
#define Get_State_Frame_Environment(o) ((o)->Specific.State_Frame.Environment)
#define Get_State_Frame_Function(o) ((o)->Specific.State_Frame.Function)
#define Get_State_Frame_Arguments(o) ((o)->Specific.State_Frame.Arg_List)
#define Get_State_Frame_State(o) ((o)->Specific.State_Frame.State)
#define Get_State_Frame_PC(o) ((o)->Specific.State_Frame.PC)
#define	Get_State_Frame_Top(o)	((o)->Specific.State_Frame.Stack_Top_Ptr)

Import	void Make_State_Frame();

/* Eclectic (special objects of one sort or another). */

typedef struct
{
	int dummy;
} Eclectic_Struct;

Import	Op_Vector Eclectic_Ops;
Import	Integer Eclectic_Print();
Import	struct Object_Struct *Eclectic_GC();

Import	Scheme_Type Eclectic_Type;

Import	struct Object_Struct *The_Rparen_Object, *The_Dot_Object;

#define Is_Eclectic(o) (Get_Type(o) == Eclectic_Type)

#define Eclectic_Size (sizeof(Eclectic_Struct)+sizeof(Common_Struct))

/* More Special objects and types. */

Import	Scheme_Type The_Undefined_Type;

/* `Any_Type' is a dummy type; it merely matches anything, for typechecking 
purposes. */

Import	Scheme_Type Any_Type;

/* StObj. */

typedef struct
{
	Integer Length;
	struct Object_Struct *Value[1];
} StObj_Struct;

Import	Op_Vector StObj_Ops;
Import	Integer	StObj_Display(), StObj_Write(), StObj_Show();
struct	Object_Struct *StObj_GC();

Import	Scheme_Type StObj_Type;

#define Is_StObj(o) (Get_Type(o) == StObj_Type)

/* The extra space for the elements (beyond the first) is allocated
    via a parameter to Make_StObj. */
#define StObj_Size(n) ((n-1)*sizeof(Object)+sizeof(StObj_Struct)\
							+sizeof(Common_Struct))

#define Get_StObj_Length(o) ((o)->Specific.StObj.Length)
#define Get_StObj_Value(o)  ((o)->Specific.StObj.Value)
#define Get_StObj_Elem(o,i) ((o)->Specific.StObj.Value[i])

Import	void Make_StObj();


/* Type "Object" is essentially a union of all of these specific sub-types. */

typedef struct Object_Struct
{
	Common_Struct Common;
	union
        {
		Boolean_Struct Boolean;
		Pair_Struct Pair;
		Empty_List_Struct Empty_List;
		Symbol_Struct Symbol;
		Number_Struct Number;
		Character_Struct Character;
		String_Struct String;
		Vector_Struct Vector;
		Procedure_Struct Procedure;
		Primitive_Struct Primitive;
		Continuation_Struct Continuation;
		Port_Struct Port;
		Eof_Struct Eof;
		Variable_Struct Variable;
		Apply_Struct Apply;
		Lambda_Struct Lambda;
		Conditional_Struct Conditional;
		Assignment_Struct Assignment;
		Definition_Struct Definition;
		Defmacro_Struct Defmacro;
		Defmacro_Call_Struct Defmacro_Call;
		Sequence_Struct Sequence;
		Delay_Struct Delay;
		Promise_Struct Promise;
		Error_Struct Error;
		Environment_Frame_Struct Environment_Frame;
		State_Frame_Struct State_Frame;
		Eclectic_Struct Eclectic;
		StObj_Struct StObj;
	} Specific;
} *Object;

Import	void Initialize_Object();

