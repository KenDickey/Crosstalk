/* architecture.c -- UMB Scheme, symbol table, stacks and heap.

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

#include <signal.h>
#include "portable.h"
#include "io.h"
#include "eval.h"
#include "object.h"
#include "architecture.h"
#include "steering.h"
#include "debug.h"
#include "primitive.h"
#include "number.h"

Public Object 	Expression_Register, Value_Register, Environment_Register, 
		Function_Register, Arguments_Register, State_Register;

Public ELabel	PC_Register;

Public	Object	Value_Debugged, State_Debugged;

Public	ELabel	PC_Debugged;

Public	Integer Arg_Stack_Ptr = 0;
Public	Integer	Debugged_Ptr  = 0;
Public	Object	Arg_Stack[ARG_STACK_SIZE];

/* Garbage collector declarations. */

Public	Boolean	Allocating = FALSE;
Public	Boolean Show_GC_Messages = TRUE;

Private Integer Free = 0;		/* First free location. */
Private Integer Working = 0;		/* Space allocations come from this. */
Private Integer Fallow = 1;		/* The half currently not in use. */
Private Byte * Heap[2] = { NULL,NULL};	/* Two spaces. */

#define INITIAL_HEAPSIZE 	200000
#define MAX_HEAPSIZE	  	800000
#define	ENLARGEMENT_FACTOR	2
#define DESIRED_RECLAIMATION	0.2

Private Integer Heapsize = INITIAL_HEAPSIZE;
Private	Boolean	Enable_Heap_Enlargement = TRUE;
Private	Integer	Next_Heapsize = INITIAL_HEAPSIZE;


typedef struct entry_structure
{
	Object Symbol;
	struct entry_structure *Next;
} Entry;


Private Entry * Make_Entry();

Private	void Init_Heap(size)

Integer size;

{
	if (Heap[0] == NULL && Heap[1] == NULL )
	{
		Heap[0] = (Byte *)malloc( (long unsigned int) (size+ALIGNMENT));
		Heap[1] = (Byte *)malloc( (long unsigned int) (size+ALIGNMENT));
	}

	if ( Heap[0] == NULL || Heap[1] == NULL )
	{
		Output( "\nPANIC: Not enough memory for the heap.\n" );
		exit(1);
	}

	Free = 0;
	Working = 0;
	Fallow = 1;
}

/* Get the current size of the heap. */

Public void Get_Heap_Size()
{
	Integer_To_Number(Heapsize - Free);
}

Public void Get_Arg_Stack_Ptr()
{
	Integer_To_Number( Arg_Stack_Ptr );
}



/* Allocate some memory from the working space. */

Public Object Allocate(size)

	Integer size;

{
	Object	new;
	void	Garbage_Collect();

	Allocating = TRUE;

	if (Free + size >= Heapsize)
	{
		/* Not enough space. */
		Garbage_Collect();
	}

	if (Free + size < Heapsize)
	{
		/* Enough space now, or was before. */
		new = (Object)&Heap[Working][Free];
#if (ALIGNMENT-1)
		/* Alignment (if not 1; defined in portable.h) */

		Free += ((size+ALIGNMENT-1)/ALIGNMENT)*ALIGNMENT;
#else
		Free += size;
#endif
	}
	else
	{
		Panic( "Memory Exhausted" );
		new =  Nil; 
	}

	if ( Control_C )
	{
		Control_C = FALSE;
		Allocating = FALSE;
		Handler( SIGINT );
	}
	Allocating = FALSE;

	return new;
}


Public void Garbage_Collect()
{
	Integer this_argument;
	Integer orig_free = Free;
	Byte	*new_heap[2] ;
	Character temp_string[120];
	
	if ( Show_GC_Messages )
	{
		Output( "GCing... " );
	}

	if ( Next_Heapsize > Heapsize )
	{
		/* Allocate a new, larger, heap */

		new_heap[0] = NULL;
		new_heap[1] = NULL;
		new_heap[0] = (Byte *) malloc((long unsigned int) Next_Heapsize);
		new_heap[1] = (Byte *) malloc((long unsigned int) Next_Heapsize);

		if ( new_heap[0] == NULL || new_heap[1] == NULL )
		{
			/* Reallocation has failed  -- Disable enlargement */

			if ( new_heap[0] != NULL ) free( new_heap[0] );
			if ( new_heap[1] != NULL ) free( new_heap[1] );
			Enable_Heap_Enlargement = FALSE;
			Next_Heapsize = Heapsize;
		}
		else
		{
			/* Enlarge the current fallow (next working) heap. */
			free( Heap[Fallow] );
			Heap[Fallow] = new_heap[Fallow];
		}
	}

	/* Exchange spaces. */
	Working = 1 - Working;
	Fallow = 1 - Fallow;

	/* Nothing's allocated yet in the new space. */
	Free = 0;

	/* Garbage collect the (object) registers. */
	Relocate(&Expression_Register);
	Relocate(&Value_Register);
	Relocate(&Environment_Register);
	Relocate(&Function_Register);
	Relocate(&Arguments_Register);
	Relocate(&State_Register);

	/* Garbage collect the (object) debugged registers */

	Relocate(&Value_Debugged);
	Relocate(&State_Debugged);

	/* And the special objects. */
	Relocate(&Nil);
	Relocate(&The_Global_Environment);
	Relocate(&The_True_Object);
	Relocate(&The_False_Object);
	Relocate(&The_Eof_Object);
	Relocate(&Current_Input_Port);
	Relocate(&Current_Output_Port);
	Relocate(&Current_Error_Port);
	Relocate(&The_Transcript_Port);
	Relocate(&The_Dot_Object);
	Relocate(&The_Rparen_Object);

	/* Debugger Registers */
	Relocate(&Traced_Procedures);

	/* And the special symbols. */
	Relocate(&The_Undefined_Symbol);
	Relocate(&The_Syntactic_Keyword);
	Relocate(&An_Argument);
	Relocate(&QUOTE_Symbol);
	Relocate(&DEFINE_Symbol);
	Relocate(&SET_Symbol);
	Relocate(&IF_Symbol);
	Relocate(&DEFMACRO_Symbol);
	Relocate(&BEGIN_Symbol);
	Relocate(&DELAY_Symbol);
	Relocate(&LAMBDA_Symbol);

	/* Now gc the stack... */
	for (this_argument = 0; this_argument < Arg_Stack_Ptr; 
				this_argument++)
	{
		Relocate(&Arg_Stack[this_argument]);
	}

	/* ...and the symbols. */
	Symbol_Hash_Iterate(Relocate);

	if ( Next_Heapsize != Heapsize )
	{
		/* Enlarge the current fallow (previous working) heap. */
		free( Heap[Fallow] );
		Heap[Fallow] = new_heap[Fallow];
		Heapsize = Next_Heapsize;
	}

	if ( Show_GC_Messages )
	{
		sprintf( temp_string,
		 "%d bytes collected, %d bytes used, heapsize %d bytes.\n", 
		 orig_free-Free, Free, Heapsize );
        	Output(temp_string);
	}

	if ( Enable_Heap_Enlargement && Heapsize < MAX_HEAPSIZE )
	{
		/* Decide whether to enlarge heap at next garbage collection */

		if ( (float) Free / (float) Heapsize > DESIRED_RECLAIMATION )
		{
			Next_Heapsize = Heapsize * ENLARGEMENT_FACTOR;
			if ( Next_Heapsize > MAX_HEAPSIZE )
			{
				Next_Heapsize = MAX_HEAPSIZE;
				if ( Show_GC_Messages )
				 Output("Disabling enlargement due to size.\n");
			}
		}
	}
}


Public void Relocate( old )

	Object * old;
{
	if ( *old != NULL )
	{
		if (Is_Forwarded(*old))
		{
			*old = Get_Forwarding_Address(*old);
		}
		else
		{
			*old = GC_Object(*old);
		}
	}
}



Public Object Move_Object(old_object, size)

	Object old_object;
	Integer size;
{
	Byte *new, *old;
	Object new_object;

	new_object = Allocate(size); /* GC never called during GC */
	old = (Byte *) old_object;
	new = (Byte *) new_object;

	for (; size > 0; size--)
	{
		*new++ = *old++;
	}

	Set_Forwarding_Address(old_object,new_object);
	return( new_object );
}

/* Symbol table/environment handling stuff. */

Public void Assign(var, value, env)
	
	Object var, value, env;
{
	Object*	location;
	Object* how;
	Integer	frame;

	if ( Is_Local_Variable( var ) )
	{
		for ( frame = 0;
		      frame < Get_Variable_Frame_Number( var );
	              frame++ ) env = Get_Environment_Frame_Previous( env );
		location = &Get_Environment_Frame_Binding_Value( env ,
				Get_Variable_Displacement( var ) );
		how = &Get_Environment_Frame_Binding_How( env ,
				Get_Variable_Displacement( var ) );
	}
	else
	{
		location = &Get_Global_Binding(Get_Variable_Symbol( var ) );
		how = &Get_Symbol_How(Get_Variable_Symbol( var ) );
	}

	if (*location == The_Undefined_Symbol)
	{
		Error1("`%s' is undefined; you can't assign to it", 
				Get_Symbol_Name(Get_Variable_Symbol(var)));
	} 

	*location = value;
	*how = Expression_Register;
	Value_Register = value;
}

Public void Define(var, value, env)

	Object var, value, env;
{
	Object*	location;
	Object* how;

	if ( Is_Local_Variable( var )  )
	{
		/* We know it's frame 0 (the most recent);
		   otherwise, compile_form() would have caught it */

		location = &Get_Environment_Frame_Binding_Value( env ,
				Get_Variable_Displacement( var ) );
		how = &Get_Environment_Frame_Binding_How( env ,
				Get_Variable_Displacement( var ) );

		if (*location != The_Undefined_Symbol)
		{
			Error1("`%s' cannot be defined twice in the same scope",
				Get_Symbol_Name(Get_Variable_Symbol( var)));
		} 
	
	}
	else
	{
		location = &Get_Global_Binding(Get_Variable_Symbol( var ) );
		how = &Get_Symbol_How(Get_Variable_Symbol( var ) );
		Get_Symbol_User_Defined(Get_Variable_Symbol( var )) = 
			Prelude_Complete;
	}

	*location = value;
	*how = Expression_Register;
	Value_Register = Get_Variable_Symbol( var );

	if ( Is_Procedure( value ) )
	{
		Get_Procedure_Name( value ) = Get_Symbol_Name( Value_Register );
	}
}

/* Extend_Environment makes a new environment to be added to the top of 
the environment stack. */

Public void Extend_Environment(actual_count)

	Integer actual_count;
{
	Object old_frame = Get_Procedure_Frame(Function_Register);
	Integer formal_count = Get_Procedure_Numargs(Function_Register);
	Boolean has_rest = Get_Procedure_Has_Rest(Function_Register);

	if (has_rest && actual_count < formal_count)
	{
		Display_Error(
		"Not enough actuals in the following procedure call", 
		Expression_Register);
	} 
	else if (! has_rest && actual_count != formal_count)
	{
		Display_Error(
		"Wrong number of actuals in the following procedure call", 
			       Expression_Register);
	}

	Environment_Register =
	    Copy_Object( old_frame, 
			 (Integer) Environment_Frame_Size( 
			     	       Get_Environment_Frame_Size(old_frame)));

	Get_Environment_Frame_Previous(Environment_Register) =
	    Get_Procedure_Environment(Function_Register);

	if (has_rest)
	{
		Integer rest_count = actual_count - formal_count;

		/* Construct the list for the rest argument. */
		Push(Nil);   /* The end of our list. */
		while (rest_count--)
		{
			Make_Pair();
			Push(Value_Register);
		}
		Get_Environment_Frame_Binding_Value(Environment_Register,
						  formal_count) = Top(1);
		Pop(1);
	}

	actual_count = formal_count;
	while (formal_count--)
	{
		Get_Environment_Frame_Binding_Value(Environment_Register, 
				 formal_count) = Top(actual_count-formal_count);
	}
	Pop(actual_count);
}


Public Object Copy_Object(old_object, size)

	Object old_object;
	Integer size;
{
	Byte *new, *old;
	Object new_object;

	Push( old_object );	/* Save from GC */
	new_object = Allocate(size);
	old = (Byte *) Top(1);
	Pop( 1 );
	new = (Byte *) new_object;

	for (; size > 0; size--)
	{
		*new++ = *old++;
	}

	return( new_object );
}


#define HASH_TABLE_SIZE 1009

Private Entry * Symbol_Hash_Table[HASH_TABLE_SIZE];


Private Integer Hash(s)

	String s;
{
	/* From Aho, Sethi and Ullman */

	String p;
	unsigned h = 0, g;
	for ( p = s ; *p != '\0' ; p++ )
	{
		h = (h << 4) + (*p);
		if ((g = (h & 0xf0000000)))
		{
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}
	return( h % HASH_TABLE_SIZE );
}

Private void Init_Hash_Table()
{
	Integer this_entry;

	for (this_entry = 0; this_entry < HASH_TABLE_SIZE; this_entry++)
		Symbol_Hash_Table[this_entry] = NULL;
}

#define hash_entry Symbol_Hash_Table[hash_value]

Public Object Intern_Name(name)

	String name;
{
	Integer hash_value = Hash(name);

	if (hash_entry == NULL)
	{
		hash_entry = Make_Entry(name);
		return hash_entry->Symbol;
	}
	else
	{
		Boolean at_end = FALSE;
		Entry * this_entry = hash_entry;

		while (strcmp(name, Get_Symbol_Name(this_entry->Symbol)) != 0
		    && !(at_end = (this_entry->Next == NULL)))
		{
			this_entry = this_entry->Next;
		}

		/* If at end of chain, add the entry. */
		if (at_end)
		{
			this_entry->Next = Make_Entry(name);
			return this_entry->Next->Symbol;
		} 
		else 
		{
			return this_entry->Symbol;
		}
	}
}

/* Go through the hash table calling the operation on all its elements. */

Public	void Symbol_Hash_Iterate(operation)

void (*operation)();			/* Takes an &object. */
{
	Integer this_bucket;
	Entry * this_entry;

	for (this_bucket = 0; this_bucket < HASH_TABLE_SIZE; this_bucket++)
	{
		for (this_entry = Symbol_Hash_Table[this_bucket];
		    this_entry != NULL;
		    this_entry = this_entry->Next )
		{
			(*operation)(&(this_entry->Symbol));
		}
	}
}


Private Entry * Make_Entry(name)

	String name;
{
	Entry * new = (Entry *) malloc(sizeof(Entry));

	if ( new == NULL )
	{
		Panic( "Out of space in making hash entry" );
	}

	Make_Symbol(name);
	new->Symbol = Value_Register;
	new->Next = NULL;

	return new;
}


/* Push and pop State Frames. */

Public void Save()
{
	Make_State_Frame();

	Get_State_Frame_Expression(Value_Register) = Expression_Register;
	Get_State_Frame_Environment(Value_Register) = Environment_Register;
	Get_State_Frame_Function(Value_Register) = Function_Register;
	Get_State_Frame_Arguments(Value_Register) = Arguments_Register;
	Get_State_Frame_PC(Value_Register) = PC_Register;
	Get_State_Frame_State(Value_Register) = State_Register;
	Get_State_Frame_Top(Value_Register) = Arg_Stack_Ptr;


	State_Register = Value_Register;
}


Public void Restore()
{
        /* Note this does not mess with the Value_Register. */

	Expression_Register = Get_State_Frame_Expression(State_Register);
	Environment_Register = Get_State_Frame_Environment(State_Register);
	Function_Register = Get_State_Frame_Function(State_Register);
	Arguments_Register = Get_State_Frame_Arguments(State_Register);
	PC_Register = Get_State_Frame_PC(State_Register);
	Arg_Stack_Ptr = Get_State_Frame_Top(State_Register);
	State_Register = Get_State_Frame_State(State_Register);
}


Public void Initialize_Architecture()
{
	Init_Heap(Heapsize);
	Init_Hash_Table();
}
