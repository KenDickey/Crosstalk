/* primitive.c -- UMB Scheme, (non-numeric) primitive procedures.

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

#include "portable.h"
#include "eval.h"
#include "object.h"
#include "architecture.h"
#include "number.h"
#include "steering.h"
#include "primitive.h"
#include "io.h"

#include <time.h>

/* Local routines. */

Private void Equal();
Private void Pair_Equal();
Private void Character_Equal();
Private void String_Equal();
Private void Vector_Equal();

/* ANSI toupper and tolower */

#define To_Lower(x) (isupper(x) ? ((x)-'A'+'a') : (x))
#define To_Upper(x) (islower(x) ? ((x)-'a'+'A') : (x))


/* Essential procedures for booleans. */

Private void Not()
{
	Value_Register = (Top(1) == The_False_Object) ? The_True_Object : 
					    The_False_Object;
}


Private void Boolean_Predicate()
{
	Value_Register = Is_Boolean(Top(1)) ? The_True_Object : 
					      The_False_Object;
}



/* The various kinds of equivalence. */

/* Two object are eq is they're the same pointers. */
Private void Eq()
{
	Value_Register = (Top(1) == Top(2)) ? The_True_Object : 
					      The_False_Object;
}

/* Eqv is eq with immutable objects (such as numbers) eqv. */
Private void Eqv()
{
	if (Get_Type(Top(1)) != Get_Type(Top(2))) 
	{
		Value_Register = The_False_Object;
	} 
	else if (Is_Number(Top(1))) 
	{
		Number_Equal();
	} 
	else if (Is_Character(Top(1))) 
	{
		Character_Equal();
	} 
	else if (Is_Vector(Top(1))
	    && Get_Vector_Length(Top(1)) == 0
	    && Get_Vector_Length(Top(2)) == 0) 
        {
		Value_Register = The_True_Object;

	} 
	else if (Is_String(Top(1))
	    && Get_String_Length(Top(1)) == 0
	    && Get_String_Length(Top(2)) == 0) 
	{
		Value_Register = The_True_Object;
	} 
	else 
	{
		Eq();
	}
}

/* Equal is eqv and it looks inside structures. */
Private void Equal()
{
	if (Get_Type(Top(1)) != Get_Type(Top(2)) )
	{
		Value_Register = The_False_Object;

	} 
	else if (Is_Pair(Top(1)))
	{
		Pair_Equal();

	} 
	else if (Is_String(Top(1)))
	{
		String_Equal();

	} 
	else if (Is_Vector(Top(1)))
	{
		Vector_Equal();

	}
	else
	{
		Eqv();
	}
}



/* Essential procedures for pairs, a.k.a. lists. */

Private void Pair_Predicate()
{
	Value_Register = Is_Pair(Top(1)) ? The_True_Object : The_False_Object;
}

Private void Cons()
{
	Push(Top(2));	/* Car */
	Push(Top(2));	/* Cdr */
	Make_Pair();
}



Private void Car()
{
	Value_Register = Get_Pair_Car(Top(1));
}


Private void Cdr()
{
	Value_Register = Get_Pair_Cdr(Top(1));
}


Private void Set_Car()
{
	Get_Pair_Car(Top(2)) = Value_Register = Top(1);
}

Private void Set_Cdr()
{
	Get_Pair_Cdr(Top(2)) = Value_Register = Top(1);
}



/* The empty list and eof. */

Private void Empty_List_Predicate()
{
	Value_Register = Is_Empty_List(Top(1))
	    ? The_True_Object : The_False_Object;
}


Private void Get_Pair_Length()
{
	Integer_To_Number(Length(Top(1)));
}



Private void Append()  /* (append obj ...)  */
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
	Integer arg_index;
	Integer length;
	Object  list;

	if (arg_count == 0)
	{
		Value_Register = Nil;
	}
	else
	{
		Value_Register = Top(1);
		arg_index = 2;

		while ( arg_index <= arg_count )
		{
			list = Top( arg_index );
			length = 0;
			
			while ( Is_Pair( list ) )
			{
				Push( Get_Pair_Car( list ) );
				list = Get_Pair_Cdr( list );
				length++;
			}

			if ( list != Nil )
				Display_Error( "Bad list argument to append: ",
					        Expression_Register );
			while ( length-- )
			{
				Push( Value_Register );
				Make_Pair();
			}
			arg_index++;
		}
	}
}


/* reverse lists */

Private void Reverse()
{
	Integer length = Length(Top(1));
	
	if (! Is_List(Top(1)))
	    Display_Error("Argument to reverse not a list", Top(1));

	Value_Register = Nil;	/* the empty list */

	while (length--)
	{
		Push(Get_Pair_Car(Top(1)));	/* car on stack first */
		Push(Value_Register);       /* cdr on stack second */
		Make_Pair(); /* pair in value register; stack restored */
		Replace(1, Get_Pair_Cdr(Top(1)));
	}
}



/* List_Tail....*/

Private void List_Tail()
{
	Integer position = 0;

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Error( "(list-tail list k) requires exact 2nd argument" );
	}
	else position = Number_To_Integer(Top(1));
        
	if (!Is_List(Top(2)))
	         Display_Error("First argument to list->tail not a list",
					     Top(2));

        if (position < 0 || position >= Length(Top(2)))
		 Display_Error("List reference out of bounds",
					      Expression_Register);
        else 
	{
	        while (position--) /* The while loop is executed as many times
				      to filter the list containing the tail  
				      elements in top(2). */
	        {
		       Replace(2,Get_Pair_Cdr(Top(2))); /* cdr below */
                }
        }  
	Value_Register = Top(2);
}

  
/* List_Ref....*/

Private void List_Ref()
{
	Integer position = 0;

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Error( "(list-ref list k) requires exact 2nd argument" );
	}
	else position = Number_To_Integer(Top(1));
        
	if (!Is_List(Top(2)))
	         Display_Error("First argument to list->tail not a list",
					     Top(2));

        if (position < 0 || position > Length(Top(2)))
		 Display_Error("List reference out of bounds",
					      Expression_Register);
        else 
	{
	        while (position--) /* The while loop is executed as many times
				      to filter kth element in car of top(2).*/
	        {
		       Replace(2,Get_Pair_Cdr(Top(2))); /* cdr below */
                }
        }  
	Value_Register = Get_Pair_Car(Top(2));
}





Private void Pair_Equal()
{
	Value_Register = The_True_Object;

	/* For efficieny's sake, use a while loop instead of recursion. 
	   (Most  pairs are lists, after all. */
	while (Value_Register == The_True_Object
	    && Is_Pair(Top(1)) && Is_Pair(Top(2)) )
	{
		Push(Get_Pair_Car(Top(1)));
		Push(Get_Pair_Car(Top(3)));
		Equal();
		Pop(2);

		Top(1) = Get_Pair_Cdr(Top(1));
		Top(2) = Get_Pair_Cdr(Top(2));
	}

	if (Value_Register == The_True_Object)
	{
		Push(Top(1));
		Push(Top(3));
		Equal();
		Pop(2);
	}
}

/* Essential procedures for symbols. */

Private void Symbol_Predicate()
{
	Value_Register = Is_Symbol(Top(1))
	    ? The_True_Object : The_False_Object;
}
/* (string->symbol str). This can make symbols of both case, unlike
   the reader, but that is according to the definition. */

Private void String_To_Symbol()
{
	Value_Register = Intern_Name(Get_String_Value(Top(1)));
}



/* (symbol->string sym). */

Private void Symbol_To_String()
{
	Make_Constant_String(Get_Symbol_Name(Top(1)));
}

/* Essential procedures for characters. */

Private void Character_Predicate()
{
	Value_Register = Is_Character(Top(1)) ? The_True_Object : 
						The_False_Object;
}




/* Actually, this isn't an essential procedure, but it's essential for 
implementing the comparison procedures! */

Private Compare_Type Character_Compare(c1, c2)
	Object c1, c2;
{
	Compare_Type answer;

	answer = Get_Character_Value(c1) < Get_Character_Value(c2)
	    ? LESS_THAN : GREATER_THAN;

	if (Get_Character_Value(c1) == Get_Character_Value(c2))
	{
		answer = EQUAL_TO;
	};

	return answer;
}

Private Compare_Type Character_CI_Compare (c1, c2)
	Object c1, c2;
{
	Compare_Type answer;

	answer = To_Lower(Get_Character_Value(c1)) <
	    To_Lower(Get_Character_Value(c2))
		? LESS_THAN : GREATER_THAN;

	if(To_Lower(Get_Character_Value(c1)) == 
	   To_Lower(Get_Character_Value(c2)))
	    answer = EQUAL_TO;

	return answer;
}


Private void Character_Equal()
{
	Value_Register = Character_Compare(Top(2), Top(1)) == EQUAL_TO
	    ? The_True_Object : The_False_Object;
}


Private void Character_Less()
{
	Value_Register = Character_Compare(Top(2), Top(1)) == LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_Greater()
{
	Value_Register = Character_Compare(Top(2), Top(1)) == GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_Less_Or_Equal()
{
	Value_Register = Character_Compare(Top(2), Top(1)) != GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_Greater_Or_Equal()
{
	Value_Register = Character_Compare(Top(2), Top(1)) != LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_CI_Equal()
{
	Value_Register = Character_CI_Compare(Top(2), Top(1)) == EQUAL_TO
	    ? The_True_Object : The_False_Object;
}


Private void Character_CI_Less()
{
	Value_Register = Character_CI_Compare(Top(2), Top(1)) == LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_CI_Greater()
{
	Value_Register = Character_CI_Compare(Top(2), Top(1)) == GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_CI_Less_Or_Equal()
{
	Value_Register = Character_CI_Compare(Top(2), Top(1)) != GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_CI_Greater_Or_Equal()
{
	Value_Register = Character_CI_Compare(Top(2), Top(1)) != LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void Character_Alpha ()
{
	Value_Register = isalpha(Get_Character_Value(Top(1))) ? 
	    The_True_Object : The_False_Object;
}


Private void Character_Numeric ()
{
	Value_Register = isdigit(Get_Character_Value(Top(1))) ? 
	    The_True_Object : The_False_Object;
}

Private void Character_WhiteSpace ()
{
	Value_Register = isspace(Get_Character_Value(Top(1))) ? 
	    The_True_Object : The_False_Object;
}

Private void Character_Upper_Case ()
{
	Value_Register = isupper(Get_Character_Value(Top(1))) ? 
	    The_True_Object : The_False_Object;
}

Private void Character_Lower_Case ()
{
	Value_Register = islower(Get_Character_Value(Top(1))) ? 
	    The_True_Object : The_False_Object;
							 
}


Private void Character_To_Integer()
{
	Make_Bignum_Number(1); /* All characters will fit in one digit. */
	Get_Number_Digits(Value_Register)[0] = (Number_Digit_Type)
	    Get_Character_Value(Top(1));
}

/* (integer->char number)return character.Strictly speaking,this takes
    any kind of number as an argument,but only a small integer is reasonable. */

Private void Integer_To_Character()
{
	 Integer n = Number_To_Integer(Top(1));
	 if (n < 0 || n >255)
	 {
	    Error("Integer value out of range to be a character");
	    return;
         };


	 Make_Character((Character) n);
}


Private void Character_To_Lower_Case ()
{
	Character c;
	c = To_Lower(Get_Character_Value(Top(1)));
	Make_Character(c);
}

Private void Character_To_Upper_Case ()
{
	Character c;
	c = To_Upper(Get_Character_Value(Top(1)));
	Make_Character(c);
}




/* Scheme strings. */

Private void String_Predicate()
{
	Value_Register = Is_String(Top(1)) ? The_True_Object : The_False_Object;
}

/* (make-string str-length fill-char) */

Private void MakeString()
{
	Character fill_char = Get_Character_Value (Top(1));
	Integer str_length = 0;
	Integer index;

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Error( "(make-string k char) requires exact 1st argument" );
	}
	else str_length = Number_To_Integer(Top(2));
        
	Make_String(str_length);	/* uninitialized string in Value_Reg */
	
	for (index = 0; index < str_length; index++)
	    Get_String_Value(Value_Register)[index] = fill_char;
	
	Get_String_Value(Value_Register)[str_length] ='\0';
}


/* (string-null str). */

Private void Is_String_Null()
{
	Value_Register = Get_String_Length(Top(1)) == 0
	    ? The_True_Object : The_False_Object;
}


/* (string-length str). */

Private void String_Length()
{
	Integer_To_Number(Get_String_Length(Top(1)));
}


/* (string-ref str position). Scheme strings are zero-origin, as in C. */

Private void String_Ref()
{
	Integer position = 0;

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Error( "(string-ref str k) requires exact 2nd argument" );
	}
	else position = Number_To_Integer(Top(1));
        
	if (position < 0 || position >= Get_String_Length(Top(2)))
		Display_Error("String reference out of bounds", 
					  Expression_Register);
	else
		Make_Character(Get_String_Value(Top(2))[position]);
}

/* (string-set! str pos c) change characters at pos to c */

Private void String_Set()
{
	Integer pos = 0; 

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Error( "(string-set! str k char) requires exact 2nd argument" );
	}
	else pos = Number_To_Integer(Top(2));
        
	if (pos < 0 || pos >= Get_String_Length(Top(3)))
	    Display_Error("String reference out of bounds", Top(2));
	
	Get_String_Value(Top(3))[pos] = 
	    Get_Character_Value(Top(1)); /* this changes the string */
	Value_Register = Top(1);

}

/* Comparisons. */

Private Compare_Type String_Compare(s1, s2)
	Object s1, s2;
{
	Integer len1 = Get_String_Length(s1);
	Integer len2 = Get_String_Length(s2);
	String  str1 = Get_String_Value(s1);
	String  str2 = Get_String_Value(s2);
	Integer shorter;

	shorter = len1 < len2 ? len1 : len2;

	while (shorter--)
	{
		if (*str1 > *str2) return GREATER_THAN;
		if (*str1 < *str2) return LESS_THAN;

		str1++;
		str2++;
	}

	return (len1 > len2) ? GREATER_THAN :
	    (len1 < len2) ? LESS_THAN :
	    EQUAL_TO;
}

Private Compare_Type String_Compare_Case_Independent(s1, s2)
	Object s1, s2;
	
{
	Integer len1 = Get_String_Length(s1);
	Integer len2 = Get_String_Length(s2);
	String  str1 = Get_String_Value(s1);
	String  str2 = Get_String_Value(s2);
	Integer shorter;

	shorter = len1 < len2 ? len1 : len2;

	while (shorter--)
	{
		if (To_Lower(*str1) > To_Lower(*str2)) return GREATER_THAN;
		if (To_Lower(*str1) < To_Lower(*str2)) return LESS_THAN;

		str1++;
		str2++;
	}

	return (len1 > len2) ? GREATER_THAN :
		(len1 < len2) ? LESS_THAN : EQUAL_TO;
}

Private void String_Equal()
{
	Value_Register = String_Compare(Top(2), Top(1)) == EQUAL_TO
	    ? The_True_Object : The_False_Object;
}

Private void String_Equal_Case_Independent()
{
	Value_Register = String_Compare_Case_Independent(Top(2), Top(1)) == 
	    EQUAL_TO ? The_True_Object : The_False_Object;
}

Private void String_Less()
{
	Value_Register = String_Compare(Top(2), Top(1)) == LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void String_Greater()
{
	Value_Register = String_Compare(Top(2), Top(1)) == GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void String_Less_Or_Equal()
{
	Value_Register = String_Compare(Top(2), Top(1)) != GREATER_THAN
	    ? The_True_Object : The_False_Object;
}

Private void String_Greater_Or_Equal()
{
	Value_Register = String_Compare(Top(2), Top(1)) != LESS_THAN
	    ? The_True_Object : The_False_Object;
}

Private void String_Less_Case_Independent()
{
	Value_Register = String_Compare_Case_Independent(Top(2), Top(1)) == 
	    LESS_THAN ? The_True_Object : The_False_Object;
}

Private void String_Greater_Case_Independent()
{
	Value_Register = String_Compare_Case_Independent(Top(2), Top(1)) == 
	    GREATER_THAN ? The_True_Object : The_False_Object;
}

Private void String_Less_Or_Equal_Case_Independent()
{
	Value_Register = String_Compare_Case_Independent(Top(2), Top(1)) != 
	    GREATER_THAN ? The_True_Object : The_False_Object;
}

Private void String_Greater_Or_Equal_Case_Independent()
{
	Value_Register = String_Compare_Case_Independent(Top(2), Top(1)) != 
	    LESS_THAN ? The_True_Object : The_False_Object;
}


/* (substring str start end). Note |start| can equal |end|, |end| just
   can't be as long as |str|. */

Private void Substring()
{
	Integer start = 0; 
	Integer end = 0; 
	Integer this_char;

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Error( "(substring str k1 k2) requires exact 2nd argument" );
	}
	else start = Number_To_Integer(Top(2));
        
	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Error( "(substring str k1 k2) requires exact 3rd argument" );
	}
	else end = Number_To_Integer(Top(1));
        
	if (start < 0 || start > end)
	{
		Display_Error("Starting position for substring out of bounds",
		    Expression_Register);
	}

	if (end > Get_String_Length(Top(3)) )
	{
		Display_Error("Ending position for substring out of bounds",
		    Expression_Register);
	}

	Make_String(end - start);

	for (this_char = start; this_char < end ; this_char++)
	{
		Get_String_Value(Value_Register)[this_char - start] =
		    Get_String_Value(Top(3))[this_char];
	}

	Get_String_Value(Value_Register)[end - start] = '\0';
}


/* (string-append str ...). */

Private void Varying_String_Append( Argnum )

	Integer Argnum;
{
	Integer count;
	Integer totlength = 0;
	Integer this_string_length;
	Boolean firstcopy = TRUE;


	count = Argnum;

	while ( count )
	{
		totlength += Get_String_Length( Top( count ) );
		count--;
	}

	Make_String( totlength );
	totlength = 0;

	while ( Argnum )
	{
		this_string_length = Get_String_Length( Top( Argnum ) );

		if ( firstcopy )
		{
			memcpy( Get_String_Value( Value_Register ),
			       Get_String_Value( Top( Argnum ) ), 
			       this_string_length );
			firstcopy = FALSE;
		}
		else
		{
			memcpy( &(Get_String_Value(Value_Register)[totlength]),
			       Get_String_Value( Top(Argnum) ), 
			       this_string_length );
		}
		totlength += this_string_length;
		Argnum--;
	}
	Get_String_Value( Value_Register )[totlength] = '\0';
}


Private void Scheme_String_Append()
{
	Varying_String_Append( Get_Apply_Numargs( Expression_Register ) );
}


Public void String_Append()  /* Append just 2 strings; used in Numbers */
{
	Varying_String_Append( 2 );
}


/* (string->list str). */

#define Top_and_Pop(x) {x = Top(1); Pop(1);}

Private void String_To_List()
{
	Integer size = Get_String_Length(Top(1));
	Object head;

	Value_Register = Nil;

	while (size--)
	{
		Push(Value_Register);
		Make_Character(Get_String_Value(Top(2))[size]);
		head = Value_Register;
		Top_and_Pop(Value_Register);
		Push(head);
		Push(Value_Register);
		Make_Pair();
	}
}



Private void List_To_String()
{
	Integer list_length = Length(Top(1));
	Integer this_element;

	if (! Is_List(Top(1)))
		Display_Error("Expected a list as parameter to list->string",
							Top(1));

	Make_String(list_length);

	for (this_element = 0; this_element < list_length; this_element++)
	{
		if (! Is_Character(First(Top(1))))
			Display_Error("Expected a character in list->string", 
							 Top(1));

		/* The |First| above will fail if Top(1) is not a list, 
		      so now |Get_Pair_Car| and |Get_Pair_Cdr| are safe. */
		Get_String_Value(Value_Register)[this_element] =
		    Get_Character_Value(Get_Pair_Car(Top(1)));
		Top(1) = Get_Pair_Cdr(Top(1));
	}

	Get_String_Value(Value_Register)[list_length] = '\0';
}

/* (string_copy string) returns a newly allocated copy of the original */

Private void String_Copy()
{
	Integer length = Get_String_Length(Top(1));
	
	Make_String(length);
	memcpy(Get_String_Value(Value_Register),
	       Get_String_Value(Top(1)), ++length);
}

/* (string_fill! string char) stores char in every element of string */
/* return value is unspecified in report */
Private void String_Fill()
{
	Integer this_char;
	Integer length = Get_String_Length(Top(2));
	Character fill = Get_Character_Value(Top(1));

	for(this_char = 0; this_char < length; this_char++)
	{
		Get_String_Value(Top(2))[this_char] = fill;
	}
	Get_String_Value(Top(2))[length] = '\0';
	Value_Register = Top(2); /* return the new string */
}



/* Vectors, a.k.a. arrays. */

Private void Vector_Predicate()
{
	Value_Register = Is_Vector(Top(1)) ? The_True_Object : The_False_Object;
}



/* (make-vector length fill) */

Private void Scheme_Make_Vector()
{
	Object fill;
	Integer length = 0;

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Error( "(make-vector k obj) requires exact 1st argument" );
	}
	else length = Number_To_Integer(Top(2));

	Make_Vector(length);
	fill = Top(1);
	while (length--)
	{
		Get_Vector_Elem( Value_Register, length ) = fill;
	}
}

   
#define check_vector_index(v, k) \
    if (k < 0 || k >= Get_Vector_Length(v)) \
      { \
      Error("Vector index is too big or too small"); \
      return; \
      } 


/* (vector-ref vec k). */

Private void Vector_Ref()
{
	Integer k = 0;

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Error( "(vector-ref vec k) requires exact 2nd argument" );
	}
	else k = Number_To_Integer(Top(1));
        
	check_vector_index(Top(2), k);
	Value_Register = Get_Vector_Elem(Top(2), k);
}


/* (vector-set! vec k elem). */

Private void Vector_Set()
{
	Integer k = 0; 

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Error( "(vector-set! vec k elem) requires exact 2nd argument" );
	}
	else k = Number_To_Integer(Top(2));
        
	check_vector_index(Top(3), k);
	Get_Vector_Elem(Top(3), k) = Value_Register = Top(1);
}


/* (vector-length vector). */

Private void Vector_Length()
{
	Integer_To_Number(Get_Vector_Length(Top(1)));
}


/* (vector-equal vec1 vec2). */

Private void Vector_Equal()
{
	if (Get_Vector_Length(Top(2)) != Get_Vector_Length(Top(1)))
	{
		Value_Register = The_False_Object;
	} 
	else
	{
		Integer this_element;

		Value_Register = The_True_Object;
		for (this_element = 0; 
		    Value_Register == The_True_Object
		    && this_element < Get_Vector_Length(Top(1));
		    this_element++)
		{
			Push(Get_Vector_Elem(Top(1), this_element));
			Push(Get_Vector_Elem(Top(3), this_element));
			Equal();
			Pop(2);
		}
	}
}


/* (vector->list vec). */

Private void Vector_To_List()
{
	Integer size = Get_Vector_Length(Top(1));

	Value_Register = Nil;

	while (size--)
	{
		Push(Get_Vector_Elem(Top(1), size));
		Push(Value_Register);
		Make_Pair();
	}
}

Public void List_To_Vector()
{
	Integer list_length = Length(Top(1));
	Integer this_element;

	if (! Is_List(Top(1)))
		  Display_Error("Expected a list as argument to list->vector",
						    Top(1));

	Make_Vector(list_length);

	for (this_element = 0; this_element < list_length; this_element++)
	{
		Get_Vector_Elem(Value_Register, this_element) = First(Top(1));
		Top(1) = Get_Pair_Cdr(Top(1));
	}
}

/* (vector-fill! vector fill) stores fill in every element of vector */
/* return value is unspecified */

Private void Vector_Fill()
{
	Integer length = Get_Vector_Length(Top(2));
        Object fill = Top(1);

	while(length--)
	{
		Get_Vector_Elem(Top(2), length) = fill;
	}
	Value_Register = Top(2); /* return new vector */
}


/* Essential procedures for procedures. */

/* The ``procedure'' object is actually not the only kind of procedure. */

Private void Procedure_Predicate()
{
	Value_Register = Is_Function(Top(1)) ? The_True_Object : 
					       The_False_Object;
}


/* (apply proc arglist). Construct an application and pass it back 
to evaluator. */

Private void Apply()
{
	if (! Is_Function(Top(2)))
	{
		Display_Error("Apply requires a function as its 1st argument",
							    Top(2));
	}

	if (! Is_List(Top(1)))
	{
		Display_Error("Apply requires a list as its 2nd argument", 
							    Top(1));
	}

	Make_Apply();
	Expression_Register = Value_Register;
	PC_Register = EVAL_EXPRESSION;
	Save();

	Push(Nil);	/* To be popped off later by Call_Primitive(). */
	Push(Nil);
}

/* (force promise). Force evaluation of promised expression,
   (built by a delay). */

Private void Force()
{
    if ( Is_Promise(Top(1)) )
    {
	if (Get_Promise_Forced(Top(1)) )
	{
	    Value_Register = Get_Promise_Expression(Top(1));
	}
	else
	{
	    Expression_Register = Top(1);
	    PC_Register = OVERWRITE_PROMISE;
	    Save();

	    Expression_Register = Get_Promise_Expression(Top(1));
	    Environment_Register = Get_Promise_Environment(Top(1));
	    PC_Register = EVAL_EXPRESSION;
	    Save();
	}
    }
    else
    {
	Value_Register = Top(1);
    }

}

/* Ports, a.k.a. input and output. */

Private void Input_Port_Predicate()
{
	Value_Register = Is_Port(Top(1)) && Is_Input_Port(Top(1))
	    ? The_True_Object : The_False_Object;
}


Private void Output_Port_Predicate()
{
	Value_Register = Is_Port(Top(1)) && Is_Output_Port(Top(1))
	    ? The_True_Object : The_False_Object;
}


/* (set-current-input-port port) */

Private void Set_Current_Input()
{
	 Input_Port_Predicate();

	 if (Value_Register == The_False_Object)
	 {
		Display_Error("Attempt to set an output port as input: ",
							Top(1));
	 }
	 Current_Input_Port = Value_Register = Top(1);
}


/* (set-current-output-port port) */

Private void Set_Current_Output()
{
	 Output_Port_Predicate();

	 if (Value_Register == The_False_Object)
	 {
		Display_Error("Attempt to set an input port as output: ",
							Top(1));
	 }
	 Current_Output_Port = Value_Register = Top(1);
}



/* (open-input-file filename). */

Private void Open_Input_File()
{
	String filename = Get_String_Value(Top(1));
	FILE *new_file;

	new_file = fopen(filename, "r");

	if (new_file == NULL)
	{
		Error1("I can't open the file `%s' for reading", filename);
	}

	Make_Port(TRUE, new_file, filename);
}


/* (open-output-file filename). */

Private void Open_Output_File()
{
	String filename = Get_String_Value(Top(1));
	FILE *new_file;

	new_file = fopen(filename, "w");

	if (new_file == NULL)
	{
		Error1("I can't open the file `%s' for writing", filename);
	}

	Make_Port(FALSE, new_file, filename);
}


/* (current-input-port). */

Private void Get_Current_Input_Port()
{
	Value_Register = Current_Input_Port;
}


/* (current-output-port). */

Private void Get_Current_Output_Port()
{
	Value_Register = Current_Output_Port;
}


/* (close-input-port port)
   (close-output-port port) ; the implementation is the same */

Private void Close_Port()
{
        if ( Get_Port_Name(Top(1)) == NULL )
	  {
            Error( "Attempt to close pipe as if it were an ordinary port." );
	  }
        fclose(Get_Port_File(Top(1)));
}

/* (read port). */

Private void Read_From_Port()
{
	Input_Port_Predicate();

	if (Value_Register == The_False_Object)
	{
		Display_Error("Attempt to read from an output port:", Top(1));
	}

	Push(Current_Input_Port);
	Current_Input_Port = Top(1);

	Read(Get_Port_File(Top(2))); /* The argument is at |Top(2)| now. */

	Current_Input_Port = Top(1);
	Pop(1);
}


/* (read-char port). */

Private void Read_Char()
{
	Character new_char;

	Input_Port_Predicate();

	if (Value_Register == The_False_Object)
		Display_Error("You're trying to read from a output port:", 
							    Top(1) );
	else
	{
		new_char = getc(Get_Port_File(Top(1)));
		if (new_char == EOF)
		{
			Value_Register = The_Eof_Object;
		}
		else
		{
			Make_Character(new_char);
		}
	}
}

Private void Char_Ready()
{

	Input_Port_Predicate();

	if (Value_Register == The_False_Object)
	    Display_Error("Attempt to apply char-ready? to an output port:", 
							    Top(1) );
	else
	{
		Value_Register = The_True_Object;  /* all ports buffered */
	}
}

/* (peek-char port). */

Private void Scheme_Peek_Char()
{
	Character new_char;

	Input_Port_Predicate();

	if (Value_Register == The_False_Object)
		Display_Error("You're trying to peek from a output port:", 
							    Top(1) );
	else
	{
		new_char = Peek_Char(Get_Port_File(Top(1)));

		if (new_char == EOF)
		{
			Value_Register = The_Eof_Object;
		}
		else
		{
			Make_Character(new_char);
		}
	}
}

Private void Eof_Predicate()
{
	Value_Register = Is_Eof(Top(1)) ? The_True_Object : The_False_Object;
}



/* (write obj port). Having a ``current output port'' makes the output routines
   simpler. But that means we have to save the old current one here. */

Public void Write_To_Port()
{
	Integer	dummy = 0;

	Output_Port_Predicate(); /* Fortunately, the port is Top(1). */

	if (Value_Register == The_False_Object)
	{
		Display_Error("Attempt to write to an input port:", Top(1));
	}

	Push(Current_Output_Port);

	/* Arguments are |Top(2)| and Top(3) now. */
	Current_Output_Port = Top(2); 

	dummy = Write_Object( Top(3) , dummy );

	Current_Output_Port = Top(1); /* Restore the old value. */
	Pop(1); /* And pop it. */
	Value_Register = Nil;
}


/* (display obj port). Similar considerations apply here. */

Private void Display_To_Port()
{
	Integer	dummy = 0;

	Output_Port_Predicate(); /* Fortunately, the port is Top(1). */

	if (Value_Register == The_False_Object)
	{
		Display_Error("Attempt to display on an input port:", Top(1));
	}

	Push(Current_Output_Port);

	Current_Output_Port = Top(2); /* The arguments moved. */
	dummy = Display_Object( Top(3) , dummy );

	Current_Output_Port = Top(1);
	Pop(1);
	Value_Register = Nil;
}


/* (write-char ch port). This just puts out the ASCII value, not
the representation of the character. */

Private void Write_Char_To_Port()
{
	Output_Port_Predicate(); /* Fortunately, the port is Top(1). */

	if (Value_Register == The_False_Object)
	{
		Display_Error("Attempt to write to an input port:", Top(1));
	}

	/* Don't need to save and restore |Current_Output_Port| here, since it's
           just a single character. */

	putc(Get_Character_Value(Top(2)), Get_Port_File(Top(1)));
	Value_Register = Nil;
}

/* (load filename). Used to load the standard prelude, hence is public. */

Public void Load()
{
	FILE *load_file;
	String load_file_name;
	Boolean save_printing_state = Get_Printing_State();

	load_file_name = Copy_String(Get_String_Value(Top(1)));
	load_file = fopen(load_file_name, "r");

	if (load_file == NULL)
	{
		Error1("I can't open `%s' for loading", load_file_name);
	}

	Output("Loading "); 
	Output(load_file_name); 
	Output("...\n");
	Set_Printing(FALSE);

	Environment_Register = The_Global_Environment; /* restored in eval.c */
	Read_Eval_Print(load_file);

	Set_Printing(save_printing_state);

	Value_Register = Nil;
}

Public void Load_Verbose()
{
	FILE *load_file;
	String load_file_name;
	Boolean save_printing_state = Get_Printing_State();

	load_file_name = Copy_String(Get_String_Value(Top(1)));
	load_file = fopen(load_file_name, "r");

	if (load_file == NULL)
	{
		Error1("I can't open `%s' for loading", load_file_name);
	}

	Output("Loading "); 
	Output(load_file_name); 
	Output("...\n");
	Set_Printing(TRUE);

	Environment_Register = The_Global_Environment; /* restored in eval.c */
	Read_Eval_Print(load_file);

	Set_Printing(save_printing_state);

	Value_Register = Nil;
}



/* Copy all input and output to a transcript file. */

/* (transcript-on filename). */

Private void Transcript_On()
{
	if (Is_Port(The_Transcript_Port))
	{
		Error("You are already making a transcript");
	} 
	else
	{
		FILE *transcript_file = fopen(Get_String_Value(Top(1)), "w");
		if (transcript_file == NULL)
		{
			Error1("I couldn't open the transcript file `%s'", 
			    Get_String_Value(Top(1)) );
		}
		Make_Port(FALSE, transcript_file, Get_String_Value(Top(1)));
		The_Transcript_Port = Value_Register;
	}
	Value_Register = Nil;
}

/* Finish up making a transcript. */
Private void Transcript_Off()
{
	if (! Is_Port(The_Transcript_Port))
	{
		Error("You're not making a transcript");
	} 
	else
	{
		fclose(Get_Port_File(The_Transcript_Port));
		The_Transcript_Port = Nil;
	}
	Value_Register = Nil;
}

/* (the-undefined-symbol) */

Private void Get_The_Undefined_Symbol()
{
	Value_Register = The_Undefined_Symbol;
}

/* (put symbol property-symbol obj) */

Private void Put()
{
	Object plist = Get_Property_List( Top(3) );
	Object prop_name = Top( 2 );
	Object obj = Top( 1 );

	while ( plist != Nil &&  Get_Pair_Car(Get_Pair_Car(plist)) != prop_name)
	{
		plist = Get_Pair_Cdr( plist );
	}

	if ( plist == Nil )
	{
		Push( prop_name );
		Push(  obj );
		Make_Pair();
		Push( Value_Register )
		    Push( Get_Property_List( Top(4) ) );
		Make_Pair();
		Get_Property_List( Top(3) ) = Value_Register;
	}
	else
	{
		Get_Pair_Cdr( Get_Pair_Car( plist ) ) = obj;
	}
	Value_Register = Top(2);
}


/* (get symbol prop-name) */

Private void Get()
{
        Object  plist = Get_Property_List( Top(2) );
        Object  prop_name = Top( 1 );

        while (plist != Nil && Get_Pair_Car(Get_Pair_Car(plist)) != prop_name)
        {
                plist = Get_Pair_Cdr( plist );
	}

        Value_Register = plist == Nil
            ? The_False_Object
	      : Get_Pair_Cdr( Get_Pair_Car( plist ) );
}

/* (gensym "prefix"). */

#define DECIMAL_NUMERALS_IN_INTEGER 10

Private void Gensym()
{
	Object prefix = Top(1);
	static Integer gensym_count = 0;
	String gen_name;
	Character count_string[DECIMAL_NUMERALS_IN_INTEGER];

	gen_name = (String) malloc(Get_String_Length(prefix)+
						 sizeof(count_string));
	if (gen_name == NULL)
	{
		Panic("Not enough memory to generate a name for gensym");
	}

	/* |prefix| might include nulls; hence, |strcpy| is not appropriate. */
	memcpy(gen_name, Get_String_Value(prefix), Get_String_Length(prefix));

	sprintf(count_string, Integer_Format, gensym_count++);

	/* This is ``memcat''. */
	memcpy(&(gen_name[Get_String_Length(prefix)]), count_string, 
	    strlen(count_string));
	gen_name[Get_String_Length(prefix)+strlen(count_string)] = '\0';

	Make_Symbol(gen_name);
	free(gen_name);
}

/* (call/cc proc). Wrap up the current continuation and call the procedure
on stack with this continuation as its single argument. */

Private void Call_CC()
{
	if (! Is_Function(Top(1)))
	{
		Display_Error("call/cc not given a function", Top(1));
	}

	Function_Register = Top(1);
	Pop(1); /* Pop here so |proc| won't be on stack of the continuation. */

	Make_Continuation();	/* Saves stack and State_Register. */

	Push(Function_Register);

	Push(Value_Register);	/* Continuation as sole argument. */
	Push(Nil);
	Make_Pair();
	Push(Value_Register);

	Make_Apply();
	Expression_Register = Value_Register;
	PC_Register = EVAL_EXPRESSION;
	Save();

	Push(Nil);	/* To be popped off later by Call_Primitive(). */
}



Private void Defmacro_Predicate()
{
  if ( Is_Defmacro( Top( 1 ) ) )

    Value_Register = The_True_Object;

  else
    if ( Is_Symbol( Top( 1 ) ) )

      Value_Register =
	/* Get the global binding for the symbol. */
	Is_Defmacro( Get_Global_Binding( Top( 1 ) ) )
	  ? The_True_Object
	  : The_False_Object;

    else
      Value_Register = The_False_Object;
}


Private void Expand_Quoted_Defmacro_Call()
{
	Object	call = Top( 1 );

	if ( ! Is_Symbol( First( call ) ) )
	{
		Display_Error( 
		"Non-defmacro name in call passed to expand-defmacro-call", 
				First( call ) );
	}

	Value_Register = Get_Global_Binding(  First( call ) );

	if (! Is_Defmacro( Value_Register ) )
	{
		Display_Error( 
		"Non-defmacro name in call passed to expand-defmacro-call", 
				First( call ) );
	}

	Push( Get_Defmacro_Transformer( Value_Register ) );
	Push( Rest( call ) );  /* Strip off the keyword from the call. */
	Make_Apply();

	Expression_Register = Value_Register;
	PC_Register = EVAL_EXPRESSION;
	Save();
}

/* Error raising routines */

Private void Scheme_Break()  /* (break) */
{
	Restore();
	Break();
}

Private void Scheme_Reset()  /* (reset) */
{
	Reset();
}

/* (edit filename). Invokes the editor specified by the EDITOR environment
   variable, else ed. */

Private void Edit()
{
	String ed = getenv("EDITOR");
	String editor;
	String command;

	editor = (ed == NULL) ? "/usr/bin/mg" : Copy_String( ed );

	command = (String) malloc(strlen(editor)+1 + 
					Get_String_Length(Top(1))+10);

	if (command == NULL)
	{
		Panic( 
		"Unable to allocate space for command in Edit() - primitive.c" );
	}

	sprintf(command, "%s %s", editor, Get_String_Value(Top(1)));

	if (system(command) != 0)
	{
		Error1("Edit on %s didn't succeed; not reloading file", 
		    Get_String_Value(Top(1)) );
	} 
	else
	{
		Load_Verbose(); /* The filename to load is still on top of the
				   stack. Default loading is verbose mode */
	}
}

Private void Edit_Silent()
{
	String ed = getenv("EDITOR");
	String editor;
	String command;

	editor = (ed == NULL) ? "/usr/bin/mg" : Copy_String( ed );

	command = (String) malloc(strlen(editor)+1 + 
					Get_String_Length(Top(1))+1);

	if (command == NULL)
	{
		Panic( 
		"Unable to allocate space for command in Edit() - primitive.c" );
	}

	sprintf(command, "%s %s", editor, Get_String_Value(Top(1)));

	if (system(command) != 0)
	{
		Error1("Edit on %s didn't succeed; not reloading file", 
		    Get_String_Value(Top(1)) );
	} 
	else
	{
		Load(); /* The filename to load is still on top of the
			   stack.*/
	}
}

Private void System()  /* (system string) */
{
        /* Call underlying system command */

        Integer completion = system( Get_String_Value(Top(1)) );
        Integer_To_Number( completion );

}

/* @@KenD@@ */
Private void Exit()
{
  Output("Scheme is Exiting now.. \n\n"); 
  exit(0);
}

Private void Time() /* (time)  */
{
        /*  seconds since 00:00:00 GMT,  Jan.  1,  1970 */

        extern time_t time( time_t * );

        Integer secs = time(NULL);
        Integer_To_Number( secs );
}


Private	void GC_Messages() 	/* (gc-messages boolean)  */
{
	Show_GC_Messages = Top(1) == The_True_Object;
}

/* Environments and Eval */


Private void Current_Environment()
{
	Value_Register = Environment_Register;
}


Private void Environment_Predicate()
{
	Value_Register = 
		Is_Environment_Frame(Top(1)) ? The_True_Object : 
			                       The_False_Object;
}


Private void Global_Environment_Predicate()
{
	Value_Register =
		(Top(1) == The_Global_Environment) ? The_True_Object : 
			                             The_False_Object;
}


Private void Evaluate()
{
	Integer arg_count = Get_Apply_Numargs(Expression_Register);

	if (arg_count == 0)
	{
		Display_Error("eval expect at least one argument",
			      Expression_Register);
	}    
	else if (arg_count == 1)
	{
	       Push(Top(1));
	       Compile_Object(Top(1));
	       Eval(Value_Register,Environment_Register);
        }
	else if (arg_count ==2)
        {
		if (! Is_Environment_Frame(Top(1)))
                    Display_Error(
			  "The second argument of eval isn't an environment" ,
			   Expression_Register);
		else
		{
			Environment_Register = Top(1);
			Push(Top(2));
			Compile_Object(Top(1));
			Eval(Value_Register,Environment_Register);
		}
	}
	else
	{
              Display_Error("Too many arguments for eval",Expression_Register);
        }
}

/* Associate Scheme symbols with all those C procedures, for all the
   essential procedures except numbers. */

Public void Initialize_Primitive()
{
	/* These are listed in the order they appear in the Scheme report. */
	Make_Primitive("not", Not, 1, Any_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	Make_Primitive("boolean?", Boolean_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("eqv?", Eqv, 2, Any_Type, Any_Type, The_Undefined_Type);
	Make_Primitive("eq?", Eq, 2, Any_Type, Any_Type, The_Undefined_Type);
	Make_Primitive("equal?", Equal, 2, Any_Type, Any_Type, 
	    The_Undefined_Type);

	Make_Primitive("pair?", Pair_Predicate, 1, Any_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("cons", Cons, 2, Any_Type, Any_Type, The_Undefined_Type);

	Make_Primitive("car", Car, 1, Pair_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	Make_Primitive("cdr", Cdr, 1, Pair_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	Make_Primitive("set-car!", Set_Car, 2, Pair_Type, Any_Type, 
	    The_Undefined_Type);

	Make_Primitive("set-cdr!", Set_Cdr, 2, Pair_Type, Any_Type, 
	    The_Undefined_Type);

	Make_Primitive("null?", Empty_List_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);


	Make_Primitive("length", Get_Pair_Length, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("append", Append, VARYING, Any_Type, The_Undefined_Type,
		       The_Undefined_Type);

	Make_Primitive("reverse", Reverse, 1, Any_Type, The_Undefined_Type,
		       The_Undefined_Type);

        Make_Primitive("list-tail",List_Tail ,2 ,Any_Type, Number_Type,
			  The_Undefined_Type);

        Make_Primitive("list-ref",List_Ref ,2 ,Any_Type, Number_Type,
			  The_Undefined_Type);

	Make_Primitive("symbol?", Symbol_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("symbol->string", Symbol_To_String, 1, Symbol_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string->symbol", String_To_Symbol, 1, String_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char?", Character_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char=?", Character_Equal, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char<?", Character_Less, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char>?", Character_Greater, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char<=?", Character_Less_Or_Equal, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char>=?", Character_Greater_Or_Equal, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char-ci=?", Character_CI_Equal, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char-ci<?", Character_CI_Less, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char-ci>?", Character_CI_Greater, 2, Character_Type, 
	    Character_Type, The_Undefined_Type);

	Make_Primitive("char-ci<=?", Character_CI_Less_Or_Equal, 2, 
	    Character_Type, Character_Type, The_Undefined_Type);

	Make_Primitive("char-ci>=?", Character_CI_Greater_Or_Equal, 2, 
	    Character_Type, Character_Type, The_Undefined_Type);

	Make_Primitive("char-alphabetic?", Character_Alpha, 1, Character_Type,
            The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char-numeric?", Character_Numeric, 1, Character_Type,
            The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char-whitespace?", Character_WhiteSpace, 1, 
	    Character_Type,The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char-upper-case?", Character_Upper_Case, 1, 
	       Character_Type,The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("char-lower-case?", Character_Lower_Case, 1, 
	       Character_Type, The_Undefined_Type, The_Undefined_Type);

        Make_Primitive("char->integer",Character_To_Integer, 1 ,
	       Character_Type, The_Undefined_Type, The_Undefined_Type);

        Make_Primitive("integer->char",Integer_To_Character ,1 , 
	       Number_Type, The_Undefined_Type,The_Undefined_Type);
	       

	Make_Primitive("char-upcase", Character_To_Upper_Case, 1, 
	       Character_Type, The_Undefined_Type, The_Undefined_Type);


	Make_Primitive("char-downcase", Character_To_Lower_Case, 1, 
	       Character_Type, The_Undefined_Type, The_Undefined_Type);


	Make_Primitive("string?", String_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive ("#_make-string", MakeString, 2, Number_Type, 
	     Character_Type, The_Undefined_Type);

	Make_Primitive("string-null?", Is_String_Null, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string-length", String_Length, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string-ref", String_Ref, 2, String_Type, Number_Type,
	    The_Undefined_Type);

	Make_Primitive ("string-set!", String_Set, 3, String_Type, Number_Type,
	    Character_Type);

	Make_Primitive("string=?", String_Equal, 2, String_Type, String_Type,
	    The_Undefined_Type);

	Make_Primitive("string-ci=?", String_Equal_Case_Independent, 2, 
	    String_Type, String_Type, The_Undefined_Type);

	Make_Primitive("string<?", String_Less, 2, String_Type, String_Type, 
	    The_Undefined_Type);

	Make_Primitive("string>?", String_Greater, 2, String_Type, String_Type, 
	    The_Undefined_Type);

	Make_Primitive("string<=?", String_Less_Or_Equal, 2, String_Type, 
	    String_Type, The_Undefined_Type);

	Make_Primitive("string>=?", String_Greater_Or_Equal, 2, String_Type, 
	    String_Type, The_Undefined_Type);

	Make_Primitive("string-ci<?", String_Less_Case_Independent, 2, 
	    String_Type, String_Type, The_Undefined_Type);

	Make_Primitive("string-ci>?", String_Greater_Case_Independent, 2, 
	    String_Type, String_Type, The_Undefined_Type);

	Make_Primitive("string-ci<=?", String_Less_Or_Equal_Case_Independent, 
	    2, String_Type, String_Type, The_Undefined_Type);

	Make_Primitive("string-ci>=?", String_Greater_Or_Equal_Case_Independent
	    , 2, String_Type, String_Type, The_Undefined_Type);

	Make_Primitive("substring", Substring, 3, String_Type, Number_Type,
	    Number_Type);

	Make_Primitive("string-append", Scheme_String_Append, VARYING, 
		       String_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string->list", String_To_List, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("list->string", List_To_String, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string-copy", String_Copy, 1, String_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("string-fill!", String_Fill, 2, String_Type,
            Character_Type, The_Undefined_Type);

	Make_Primitive("vector?", Vector_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_make-vector", Scheme_Make_Vector, 2, Number_Type,
	    Any_Type, The_Undefined_Type);

	Make_Primitive("vector-length", Vector_Length, 1, Vector_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("vector-ref", Vector_Ref, 2, Vector_Type, Number_Type, 
	    The_Undefined_Type);

	Make_Primitive("vector-set!", Vector_Set, 3, Vector_Type, Number_Type, 
	    Any_Type);

	Make_Primitive("vector->list", Vector_To_List, 1, Vector_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("list->vector", List_To_Vector, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);
	
	Make_Primitive("vector-fill!", Vector_Fill, 2, Vector_Type,
            Any_Type, The_Undefined_Type);

	Make_Primitive("procedure?", Procedure_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	/* apply does its own type checking. */

	Make_Primitive("#_apply", Apply, 2, Any_Type, Any_Type, 
	    The_Undefined_Type);

	Make_Primitive("force", Force, 1, Any_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	Make_Primitive("call-with-current-continuation", Call_CC, 1, Any_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("input-port?", Input_Port_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("output-port?", Output_Port_Predicate, 1, Any_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("current-input-port", Get_Current_Input_Port, 0,
	    The_Undefined_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("current-output-port", Get_Current_Output_Port, 0,
	    The_Undefined_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("set-current-input-port!" , Set_Current_Input, 1 ,
		Port_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("set-current-output-port!" , Set_Current_Output, 1 ,
		Port_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("open-input-file", Open_Input_File, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("open-output-file", Open_Output_File, 1, String_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("close-input-port", Close_Port, 1, Port_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("close-output-port", Close_Port, 1, Port_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_read", Read_From_Port, 1, Port_Type, 
		The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_read-char", Read_Char, 1, Port_Type, 
		The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_char-ready?", Char_Ready, 1, Port_Type, 
		The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_peek-char", Scheme_Peek_Char, 1, Port_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("eof-object?", Eof_Predicate, 1, Any_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_write", Write_To_Port, 2, Any_Type, Port_Type, 
	    The_Undefined_Type);

	Make_Primitive("#_display", Display_To_Port, 2, Any_Type, Port_Type,
	    The_Undefined_Type);

	Make_Primitive("#_write-char", Write_Char_To_Port, 2, Character_Type,
	    Port_Type, The_Undefined_Type);

	Make_Primitive("load", Load, 1, String_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	Make_Primitive("loadv", Load_Verbose, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("transcript-on", Transcript_On, 1, String_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("transcript-off", Transcript_Off, 0, The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("the-undefined-symbol", Get_The_Undefined_Symbol, 0, 
	    The_Undefined_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive( "put", Put, 3, Symbol_Type, Symbol_Type, Any_Type );

	Make_Primitive( "get", Get, 2, Symbol_Type, Symbol_Type, 
	    The_Undefined_Type);

	Make_Primitive("gensym", Gensym, 1, String_Type, The_Undefined_Type, 
	    The_Undefined_Type);

	/* call/cc does its own type checking. */

	Make_Primitive("call/cc", Call_CC, 1, Any_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("expand1-quoted-defmacro-call",
		       Expand_Quoted_Defmacro_Call, 1, 
		       Pair_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("defmacro?", Defmacro_Predicate, 1,
		       Any_Type, The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_break", Scheme_Break, 0, The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("reset", Scheme_Reset, 0, The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_edit", Edit, 1, String_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("#_edits", Edit_Silent, 1, String_Type, 
            The_Undefined_Type, The_Undefined_Type);

/* @@KenD@@ */
        Make_Primitive("exit", Exit, 0, The_Undefined_Type);

        Make_Primitive("system", System, 1, String_Type,
            The_Undefined_Type, The_Undefined_Type);

        Make_Primitive("time", Time, 0, The_Undefined_Type,
            The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("gc-messages", GC_Messages, 1, Boolean_Type, 
            The_Undefined_Type, The_Undefined_Type);

	Make_Primitive( "current-environment", Current_Environment, 0,
                   The_Undefined_Type, The_Undefined_Type, The_Undefined_Type);
	Make_Primitive( "environment?", Environment_Predicate, 1,
                   Any_Type, The_Undefined_Type, The_Undefined_Type);
	Make_Primitive( "global-environment?", Global_Environment_Predicate, 1,
                   Any_Type, The_Undefined_Type, The_Undefined_Type);
	Make_Primitive( "eval", Evaluate, VARYING,
                   Any_Type, The_Undefined_Type, The_Undefined_Type);

	/* Defined in architecture.c. */

	Make_Primitive("heap-size", Get_Heap_Size, 0, The_Undefined_Type,
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("arg-stack-ptr" , Get_Arg_Stack_Ptr, 0, 
	    The_Undefined_Type, The_Undefined_Type, The_Undefined_Type);
}
