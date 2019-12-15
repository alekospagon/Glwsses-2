#include <stdio.h>
#include <stdlib.h>
#include <time.h>


/*
 *  __                _____                                     
 * /\ \____  ______  /\  __\ __  __  ______  ______  ______    
 * \ \  __ \/\  __ \ \_\ \__/\ \ \ \/\  __ \/\  __ \/\  __ \   
 *  \ \ \_\ \ \  __/_/\__  _\ \ \_\ \ \ \ \ \ \ \_\ \ \  __/_   
 *   \ \_____\ \_____\/_/\ \/\ \_____\ \_\ \_\ \____ \ \_____\ 
 *    \/_____/\/_____/  \ \_\ \/_____/\/_/\/_/\/\_____\/_____/
 *                       \/_/                  \/_____/
 * 
 */


/* =================== PARAMETERS ==================== */

#define STACK_MAX 	1024 * 1024 			//STACK SIZE 2^20 lekseis
#define HEAP_MAX  	1024 * 1024 * 16		//HEAP  SIZE 2^24 lekseis

#define marked    	1		//marked cons -- for mark and sweep
#define unmarked  	0		

#define pointer   	1		//myint is integer or pointer to a cons
#define integer   	0



/* ============== STACK ELEMENTS AND VM ============== */

/* * * * STACK ELEMENTS * * * */
//Bitfield --> split 8-byte 
typedef struct {
	int mark : 1;					//one bit for mark -> 1 means marked
	int type : 1;					//one bit for type -> 1 means ptr to CONS
	signed long int value : 62 ;	//rest 62 bits are value -> int or ptr to cons
} myint;


/* * * * HEAP ELEMENTS * * * */
//Cons
typedef struct {				
	myint head;						//contains two stack elements
	myint tail;
} cons;




//VM 
typedef struct h_vm {	
	//BEFUNGE STACK
	myint stack[STACK_MAX];		//BEFUNGE-93 STACK ints or ptrs to heap
	int stackSize;				//Array and int implementing fast vector
	
	
	//HEAP
	cons  heap [HEAP_MAX];		//cons are stored here.
	
	
	//STACK IMPLEMENTED WITH ARRAY & INDEX
	//FETCHES FREE ADDRESSES ON THE HEAP
	int indexes[HEAP_MAX];		//indexes on heap with free space
	int idx_ptr;				//indexes and this int implement vector
} VM;



/* =============== FUNCTION PROTOTYPES ===============*/

//create and destroy VM
VM * VM_create		();									//MALLOC VM
void VM_freeVM		(VM* vm);							//FREE   VM

//heap management
void VM_mark		(myint elem);						//MARK A CONS
void VM_markAll		(VM* vm);							//MARK REACHABLE
void VM_sweep		(VM* vm);							//SWEEP MARKED
void VM_gc			(VM* vm);							//MARKALL + SWEEP


//stack management
myint VM_pop		(VM* vm);							//POP  A STACK ELEM
void VM_push		(VM* vm, myint 	to_be_pushed);		//PUSH A STACK ELEM
void VM_pushInt		(VM* vm, signed long int intValue);	//PUSH SIMPLE INT
void VM_pushPair	(VM* vm);							//PUSH PAIR - c command
void VM_pushHead	(VM* vm);							//PUSH HEAD - h command
void VM_pushTail	(VM* vm);							//PUSH TAIL - t command

/* =================== VM FUNCTIONS =================== */


//Yo pass me a VM 
VM* VM_create() {
	VM *vm = (VM *)malloc(sizeof(VM));	//kane xwro

	for(int i = 0; i < HEAP_MAX; i++)	//all heap is free to use
		vm->indexes[i] = i;				//index i on heap is free. 

	vm->stackSize   = 0;				//ka8olou stack
	vm->idx_ptr     = HEAP_MAX - 1;		//all adresses free at the beggining

	return vm;
}


//den mas xwraei kai tous duo auth h mnhmh
void VM_freeVM(VM *vm) {
	free(vm);
}




/* ================= HEAP MANAGEMENT ================== */


//O xafies tou VM
void VM_mark(myint elem) {
	if(elem.type == pointer){						//Ean einai deikths se cons
		
		cons * cons_ptr = (cons *) elem.value;		//phgaine sto cons
		
		//if marked return -> avoid circles
		if(cons_ptr->head.mark == marked)			//an einai markarismeno
			return;									//fuge
		
		//else mark and recurse
		cons_ptr->head.mark = marked;				//alliws parkare ta stoixeia
		cons_ptr->tail.mark = marked;
		
		//mark head and tail
		VM_mark(cons_ptr->head);					//markare anadromika giati an to cons
		VM_mark(cons_ptr->tail);					//einai accessible kai ta h-t einai
	}
}


//bgale oti mporeis gia na perasei to skoupidiariko
void VM_markAll(VM* vm){
	for(int i = 0; i < vm->stackSize; i++) {		//mark all reachable
		VM_mark(vm->stack[i]);
	}
}



//SKOUUUUPA
void VM_sweep(VM * vm){
	
	//iterate olo to heap
	for(int heap_idx = 0; heap_idx < HEAP_MAX; heap_idx++){
		
		//check head or tail. if one is marked they are both marked
		if(vm->heap[heap_idx].head.mark == unmarked)
			//eleu8erwse xwro: apla dwse to index sou pisw
			vm->indexes[++vm->idx_ptr] = heap_idx; //free index
			
		else
			//an einai markarismena: ksemarkare ta gia to 
			//epomeno collect
			vm->heap[heap_idx].head.mark = unmarked;
			vm->heap[heap_idx].tail.mark = unmarked;
	}
}


//kane ena ka8arisma giati 8a fwnazei h mama
void VM_gc(VM* vm) {
	VM_markAll(vm);	//ti mporw na ka8arisw?
	VM_sweep(vm);	//as to ka8arisw
}




/* ================= STACK MANAGEMENT ================= */

//Ena object apo thn stoiva 8a h8ela parakalw
myint VM_pop(VM * vm) {
	//vevaiws. oriste to object sas
	if(vm->stackSize) 
		return vm->stack[vm->stackSize--];
	
	
	// !!! H befunge leei se stack overflow epistrefei 0
	
	myint  res = {			//ftiakse ena res sta grhgora
		.mark = unmarked,
		.type = integer, 
		.value = 0
	};
	
	return res;
}



//push ena tupou myint pou exw etoimo:
//sthn ousia mono me command duplicate (:), swap(\)
//kai cons, head, tail exw myint. alliws to programma
//kanei push aplous akeraious me allo handle pou dinw
void VM_push(VM * vm, myint to_be_pushed) {
	//poumwse to stack
	if(vm->stackSize > STACK_MAX){				//case of overflow
		printf("Stack overflow");
		exit(1);
	}
	
	//PUSH INTO STACK
	vm->stack[++vm->stackSize] = to_be_pushed;	//pass to stack
}



//sprwkse enan akeraio sthn stoiva
void VM_pushInt(VM* vm, signed long int intValue) {
	//Ftiaxnw myint me thn timh mou
	myint pushed_int = {
		.mark = unmarked, 
		.type = integer, 
		.value = intValue
	};
	
	//PUSH INTO STACK
	VM_push(vm, pushed_int);
}



//sprwkse ena "CONS" sthn stoiva
void VM_pushPair(VM* vm) {
	//TRIGGER GC
	if (vm->idx_ptr == -1) 						//all addresses being used
		VM_gc(vm);								//COLLECT


	//MAKE PAIR
	int index = vm->indexes[vm->idx_ptr--];		//fetch an index to free memory on the heap
	
	//point to heap
	cons * cons_ptr = &(vm->heap[index]);		//address on heap
	cons_ptr->tail      = VM_pop(vm);			//put values in
	cons_ptr->head      = VM_pop(vm);

	//MAKE STACK VALUE
	myint pushed_ptr = {
		.mark = unmarked, 
		.type = pointer, 
		.value = (signed long int) cons_ptr		//save pointer to heap
	};											//ptr on heap -> low memory -> 62bit: enough

	//PUSH INTO STACK
	VM_push(vm , pushed_ptr);
}



//h command
void VM_pushHead(VM* vm){
	//pop a cons pointer
	myint popped = VM_pop(vm);
	
	//MAKE SURE ITS CONS POINTER
	if(popped.type == integer){
		printf("Head command on simple int. exiting \n");
		exit(1);
	}
	
	//take the pointer
	cons * cons_ptr = (cons *) popped.value;
	//push the head
	VM_push(vm, cons_ptr->head);
}



//t command
void VM_pushTail(VM* vm){
	//pop a cons pointer
	myint popped = VM_pop(vm);
	
	//MAKE SURE ITS CONS POINTER
	if(popped.type == integer){
		printf("Tail command on simple int. exiting \n");
		exit(1);
	}
	
	//take the pointer
	cons * cons_ptr = (cons *) popped.value;
	//push the tail
	VM_push(vm, cons_ptr->tail);
}








