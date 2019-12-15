#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "vm_files.h"	//VM HELPING FILES


//GRID SIΖΕ
#define X 80
#define Y 25


//JUMP TO INSTRUCTION IMPLEMENTATION
#define EXECUTE  goto *(labels[com])
//MOVE INSTRUCTION POINTER
#define CONTINUE goto MOVE
//FETCH NEW COMMAND
#define FETCH    goto NEW_COMMAND

//map directions 
#define Right 0
#define Left  1
#define Up    2
#define Down  3


/* ================= GRID PARAMS =================*/

static uint_fast8_t x_pos = 0, y_pos = 0; 		//position in grid
static uint_fast8_t x_max = 0, y_max = 0;		//dont go to EMPTY space 
												//-- grid may be larger

static uint_fast8_t dir; 						//Direction IP is moving 

static uint_fast8_t string_mode = 0;			//toggles string mode




//GRID AND VM
static char ins[Y][X]; 							//instructions (the grid)
static VM * vm;									//memory given (the _VM_)





/* =============== FUNCTION PROTOTYPES ==============*/

//grid functions
static void 			init_grid 	(FILE * fd);			//read grid
static void 			print_grid	();						//debug show grid

/* ---- PUSH - POP FUNCS ---- */
//ARITHMETIC AND LOGIC --> when I only care for value
static signed long int	al_pop 		();						//pop int - not myint
static void 			al_push		(signed long int val);	//push int->VM handles it

//READ PUSH_&_POP --> for swap, duplicate, c, h, t
static myint			my_pop 		();						//pop  myint
static void 			my_push		(myint a);				//push myint

//commands c, h , t
static void 			pushPair	();						//special commands
static void 			pushHead	();
static void 			pushTail	();





/* ===============  GRID INIT AND HELPERS =============== */

//debug. 
static void print_grid(){
	for(int i = 0; i < Y; i++) {
		for(int j = 0; j < X; j++) 
			printf("%c", ins[i][j]);
		printf("\n");
	}
}


//READ GRID FROM OPEN FILE
static void init_grid(FILE * fd) {
	char tmp = ' ';
	int_fast8_t x,y;

	//INIT BLANK
	for(y = 0; y < Y; y++)		//fill with space (if that can be done :p )
		for(x = 0; x < X; x++)	
			ins[y][x] = ' ';


	//READ LINE
	for(y = 0; 1; y++ ) 
	{ 
		x = 0;
		//READ BYTES
		while( (tmp = fgetc(fd)) != EOF) 
		{ 
			//READ UNTIL NEW LINE CHAR
			if(tmp != '\n')
			{
				ins[y][x++] = (char)tmp;		//store
				x_max = x > x_max ? x : x_max;	//strech horizontal limit
			}
			else
				break;
		}
		
		if( tmp == EOF )	//EOF FOUND
			break;
	}
	
	
	y_max = y;	//strech vertival limit

	//GO AWAY FILE
	fclose(fd); //Files have feelings :(
}





/* ================= HANDLES TO VM ================= */

//ARITHMETIC AND LOGIC PUSH_&_POP -> handles simple integers
static signed long int al_pop() {		//programmer sees only value field
	return VM_pop(vm).value;
}


static void al_push(signed long int val) {
	VM_pushInt(vm, val);
}




//GENERAL PUSH_&_POP -> handles myint (mark and type keeped) -> swap, duplicate etc.
static myint my_pop() {
	return VM_pop(vm);
}


static void my_push(myint a) {
	VM_push(vm, a);
}


//special commands
static void pushPair() {
	VM_pushPair(vm);
}

static void pushHead() {
	VM_pushHead(vm);
}

static void pushTail() {
	VM_pushTail(vm);
};






//general purpose (register like) -- global 
static int_fast64_t n1,n2,n3;			//3 signed long ints	
static myint obj1, obj2;				//2 myints : swap, dup, pop
static char c;							//char commands, read, write, get, put, ..
static uint_fast8_t x_pos_, y_pos_;		//my position at the grid
static char com = ' ';					//current instruction



//function that moves IP. called only from '#' command
//rest of program executes this routine without calling this func
//to make it faster --> calls cost (OTE principle)
static void move() {
	switch(dir) {
		case Right:	
			if(x_pos == x_max)		//out of limit: wrap
				x_pos = 0;
			else 
				x_pos += 1;
			break;

		case Left:	
			if(x_pos)
				x_pos -= 1;
			else
				x_pos = x_max-1;	//out of limit: wrap
			break;

		case Down:
			if(y_pos == y_max)		//out of limit: wrap
				y_pos = 0;
			else
				y_pos += 1;
			break;

		case Up:	
			if(y_pos)
				y_pos -= 1;
			else
				y_pos = y_max-1;	//out of limit: wrap
			break;
	}
}


/* ================= MAIN. READ & EXEC ================= */

int main(int argc, char ** argv)
{
	//OPEN FILE AND INIT GRID
	FILE * fd;
	
	if(argc == 1){
		printf("error: no code file given\n");
		printf("Usage: ./exec grid_file.txt \n");
		exit(1);
	}
	
	
	if((fd = fopen(argv[1] , "r" )) == NULL){
		printf("error opening file. exiting...\n");
		exit(1);
	}
	
	
	//READ GRID
	init_grid( fd );

	//CREATE A VM
	vm = VM_create();


	/* =========== COMMAND LABELS =========== */
	/* ========= INDIRECT THREADED ========== */
	static void *labels[128];
	for(int i = 0; i<128; i++)
		labels[i] = &&UNKNOWN;
	
	//ARITHMETIC AND LOGIC COMMANDS
	labels['+'] = &&ADD;
	labels['-'] = &&SUB;
	labels['*'] = &&MUL;
	labels['/'] = &&DIV;
	labels['%'] = &&MOD;
	labels['`']  = &&GT;
	labels['!'] = &&NOT;
	
	//CONTROL FLOW
	labels['>'] = &&RIGHT;
	labels['<'] = &&LEFT;
	labels['^'] = &&UP;
	labels['v'] = &&DOWN;
	labels['?'] = &&RAND;
	labels['_'] = &&H_IF;
	labels['|'] = &&V_IF;
	
	//ONLY STACK BASED
	labels['\\'] = &&SWAP;
	labels[':'] = &&DUP;
	labels['$'] = &&POP;	
	labels['0'] = &&PUT0;	//push digits
	labels['1'] = &&PUT1;
	labels['2'] = &&PUT2;
	labels['3'] = &&PUT3;
	labels['4'] = &&PUT4;
	labels['5'] = &&PUT5;
	labels['6'] = &&PUT6;
	labels['7'] = &&PUT7;
	labels['8'] = &&PUT8;
	labels['9'] = &&PUT9;

	//GRID COMMANDS
	labels['g'] = &&GET;
	labels['p'] = &&PUT;
	labels['#'] = &&SKIP;
	labels[' '] = &&SPACE;

	//IO
	labels['.'] = &&PR_INT;
	labels[','] = &&PR_CHAR;
	labels['&'] = &&G_INT;
	labels['~'] = &&G_CHAR;
	labels['"'] = &&STR;
		
	//SPECIAL COMMANDS
	labels['c'] = &&CONS;
	labels['h'] = &&HEAD;
	labels['t'] = &&TAIL;

	//HALT
	labels['@'] = &&HALT;

	//INITIALLY GO RIGHT
	dir = Right;



	NEW_COMMAND:
		//FETCH COMMAND
		com = ins[y_pos][x_pos];

		//CASE: STRING MODE
		if(string_mode == 1)
			if(com != '"') {
				al_push((int_fast64_t) com);
				CONTINUE;
			}

		
	
		// ************** EXECUTE ************** //
		EXECUTE;	


		/* ========== ARI8MHTIKES ENTOLES ========== */
		ADD:
			n2 = al_pop();
			n1 = al_pop();
			al_push(n1 + n2);
		CONTINUE;


		SUB:
			n2 = al_pop();
			n1 = al_pop();
			al_push(n1 - n2);
		CONTINUE;


		MUL:
			n2 = al_pop();
			n1 = al_pop();
			al_push(n1 * n2);
		CONTINUE;


		DIV:
			n2 = al_pop();
			n1 = al_pop();
			al_push(n1 / n2);
		CONTINUE;


		MOD:
			n2 = al_pop();
			n1 = al_pop();
			al_push(n1 % n2);
		CONTINUE;


		
		GT:	//greater than
			n2 = al_pop();
			n1 = al_pop();
			if (n1 > n2)
				al_push(1);
			else
				al_push(0);
		CONTINUE;


		NOT:
			n1 = al_pop();
			if( n1 == 0 )
				al_push(1);
			else
				al_push(0);
		CONTINUE;

		/* ========== CONTROL FLOW COMMS ========== */


		RIGHT:	
			dir = Right;
		CONTINUE;

		LEFT:
			dir = Left;
		CONTINUE;

		UP:
			dir = Up;
		CONTINUE;

		DOWN:
			dir = Down;
		CONTINUE;

		RAND:
			dir = rand() % 4;
		CONTINUE;


		H_IF:	//HORIZONTAL IF
			n1 = al_pop();
		
			if(n1 == 0)
				dir = Right;
			else 
				dir = Left;
		CONTINUE;


		V_IF:	//VERTICAL IF
			n1 = al_pop();

			if(n1 == 0)
				dir = Down;
			else
				dir = Up;
		CONTINUE;


		/* ========== STACK BASED COMMS ========== */
		
		SWAP:	//swap 
			obj2 = my_pop();		//my_pop oxi al_pop
			obj1 = my_pop();		//an einai deikths se cons
									//to krataw opws einai
			my_push(obj2);
			my_push(obj1);
		CONTINUE;
		
		
		DUP:	//DUPLICATE
			obj1 = my_pop();		//keep type and mark
		
			my_push(obj1);
			my_push(obj1);
		CONTINUE;
		
		
		POP:	//POP & DO NOTHING
			obj1 = my_pop();
		CONTINUE;
		
		
		//push digit
		PUT0: al_push(0); CONTINUE;
		PUT1: al_push(1); CONTINUE;
		PUT2: al_push(2); CONTINUE;
		PUT3: al_push(3); CONTINUE;
		PUT4: al_push(4); CONTINUE;
		PUT5: al_push(5); CONTINUE;
		PUT6: al_push(6); CONTINUE;
		PUT7: al_push(7); CONTINUE;
		PUT8: al_push(8); CONTINUE;
		PUT9: al_push(9); CONTINUE;
		
		
		/* ========== GRID COMMANDS ========== */
		
		GET:	//get command
			n2 = al_pop();
			n1 = al_pop();
			x_pos_ = (int_fast64_t) n1;
			y_pos_ = (int_fast64_t) n2;
			n3 = (int_fast64_t) ins[y_pos_][x_pos_]; //get command
			al_push(n3);
		CONTINUE;
		


		PUT:	//CHANGE GRID
			n3 = al_pop();
			n2 = al_pop();
			n1 = al_pop();
			
			c      = (char) n1;
			x_pos_ = (uint_fast8_t)  n2;
			y_pos_ = (uint_fast8_t)  n3;
			
			if(x_pos_ >= X || y_pos_ >= Y){
				printf("Command p, out of grid: x = %d, y = %d\n", x_pos_, y_pos_);
				exit(1);
			}
				
			//STRECH GRID
			y_max = y_max > y_pos_ + 1 ? y_max : y_pos_ + 1;
			x_max = x_max > x_pos_ + 1 ? x_max : x_pos_ + 1;
				
				
			ins[y_pos_][x_pos_] = c;
		CONTINUE;


		SKIP:	//SKIP COMMAND -- move once more
			move();
		CONTINUE;


		SPACE:	//IGNORE
		CONTINUE;

		/* ========== INPUT & OUTPUT ========== */


		PR_INT:	//PRINT AS INT		
			n1 = al_pop();
			printf("%d", (int) n1);
			fflush(stdout);
		CONTINUE;


		PR_CHAR:	//PRINT AS CHAR	
			n1 = al_pop();
			printf("%c", (char) n1);
			fflush(stdout);
		CONTINUE;


		G_INT:	//TAKE INT INPUT	
			scanf("%ld", &n1);
			al_push(n1);
		CONTINUE;


		G_CHAR:	//TAKE CHAR INPUT
			scanf("%c", &c);
			al_push((int_fast64_t) c);
		CONTINUE;
		
		
		STR:	//TOGGLE STRING MODE
			string_mode = 1 - string_mode;
		CONTINUE;
	
	
		/* ========== SPECIAL COMMANDS ========== */	
		
		CONS:
			pushPair();
		CONTINUE;

		HEAD:
			pushHead();
		CONTINUE;

		TAIL:
			pushTail();
		CONTINUE;
		
		
		/* ================ HALT ================ */
		
		HALT: 
			VM_freeVM(vm);
			return 0;
		
		/* ============ WRONG COMMAND =========== */
		
		UNKNOWN:
			printf("UNKNOWN COM(ASCII): %d\n", com);
			fflush(stdout);

		/* ========== MOVE INS POINTER ========== */
		MOVE:
			switch(dir) {
				case Right:	
					if(x_pos == x_max)
						x_pos = 0;
					else 
						x_pos += 1;
					break;

				case Left:	
					if(x_pos)
						x_pos -= 1;
					else
						x_pos = x_max-1;
					break;

				case Down:
					if(y_pos == y_max)
						y_pos = 0;
					else
						y_pos += 1;
					break;

				case Up:	
					if(y_pos)
						y_pos -= 1;
					else
						y_pos = y_max-1;
					break;
			}
			

		FETCH;	//GET NEXT COMMAND
	

	return 0;
}
