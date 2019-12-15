#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define STACK_MAX 1024 * 256


#define FETCH    goto NEW_COMMAND
#define EXECUTE  goto *(void *) labels[com];
#define CONTINUE goto MOVE

//map directions 
#define Right 0
#define Left  1
#define Up    2
#define Down  3

//grid max size
#define Y 25
#define X 80


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


/* =================== PARAMETERS ===================*/

//___GLOBAL PARAMETERS___
//SO FUNCTIONS DONT HAVE 42 ARGUMENTS
static uint_fast8_t x_pos = 0, y_pos = 0; 		//position in grid
static uint_fast8_t x_max = 0, y_max = 0;		//dont go to EMPTY space 
							//-- grid may be larger
												
static uint_fast8_t dir = Right;			//Direction IP is moving 
static uint_fast8_t string_mode = 0;			//toggles string mode



//GRID AND STACK
signed long int stack[STACK_MAX];
int stackSize = 0;

static char ins[Y][X]; 					//instructions (the grid)



/* =============== FUNCTION PROTOTYPES ==============*/

//grid functions
void 			init_grid 	(FILE * fd);
void 			print_grid	();

//weird pop
signed long int		my_pop 		();

//go to next instruction
void 			move		();



/* =================== GRID FUNCS ===================*/

void print_grid(){
	for(int i = 0; i < Y; i++) {			//Iter lines
		for(int j = 0; j < X; j++)		//Iter chars
			printf("%c", ins[i][j]);	//print char
		printf("\n");
	}
}


//READ GRID FROM OPEN FILE
void init_grid(FILE * fd) {
	char tmp = ' ';
	int_fast8_t x,y;

	//INIT BLANK
	for(y = 0; y<Y; y++)		//fill with space (if that can be done :p )
		for(x = 0; x<X; x++)	
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
				ins[y][x++] = (char)tmp;	//store
				x_max = x > x_max ? x : x_max;	//strech horizontal lim
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



/* =================== BEFUNGE POP ===================*/

//BEFUNGE STACK WEIRD POP
signed long int my_pop(){
	if(stackSize > 0)			//official site said so
		return stack[--stackSize];
	else 					//case of underflow
		return 0;
}

void my_push(signed long int n) {
	if(stackSize > STACK_MAX){
		printf("Stack overflow");
		exit(1);
	}
	stack[stackSize++] = n;
}

//general purpose (register like) -- global 
signed long int n1,n2,n3;		//3 signed long ints
char c;					//char commands, read, write, get, put, ..
uint_fast8_t x_pos_, y_pos_;		//my position at the grid
char com = ' ';				//current instruction



//function that moves IP. called only from '#' command
//rest of program executes this routine without calling this func
//to make it faster -- OTE principle: calls cost
void move() {
	switch(dir) {
		//no modulo --> costs. if at edge -> wrap
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
				my_push((int_fast64_t) com);
				CONTINUE;
			}

		
	
	
		EXECUTE;	// ************** EXECUTE ************** //




		/* ========== ARI8MHTIKES ENTOLES ========== */
		ADD:
			n2 = my_pop();
			n1 = my_pop();
			my_push(n1 + n2);
		CONTINUE;


		SUB:
			n2 = my_pop();
			n1 = my_pop();
			my_push(n1 - n2);
		CONTINUE;


		MUL:
			n2 = my_pop();
			n1 = my_pop();
			my_push(n1 * n2);
		CONTINUE;


		DIV:
			n2 = my_pop();
			n1 = my_pop();
			my_push(n1 / n2);
		CONTINUE;


		MOD:
			n2 = my_pop();
			n1 = my_pop();
			my_push(n1 % n2);
		CONTINUE;


		
		GT:	//greater than
			n2 = my_pop();
			n1 = my_pop();
			if (n1 > n2)
				my_push(1);
			else
				my_push(0);
		CONTINUE;


		NOT:
			n1 = my_pop();
			if( n1 == 0 )
				my_push(1);
			else
				my_push(0);
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
			n1 = my_pop();
		
			if(n1 == 0)
				dir = Right;
			else 
				dir = Left;
		CONTINUE;


		V_IF:	//VERTICAL IF
			n1 = my_pop();

			if(n1 == 0)
				dir = Down;
			else
				dir = Up;
		CONTINUE;



		/* ========== STACK BASED COMMS ========== */
		
		
		SWAP:	//swap 
			n2 = my_pop();		//my_pop oxi al_pop
			n1 = my_pop();		//an einai deikths se cons
						//to krataw opws einai
			my_push(n2);
			my_push(n1);
		CONTINUE;
		
		
		
		DUP:	//DUPLICATE
			n1 = my_pop();		//keep type and mark
		
			my_push(n1);
			my_push(n1);
		CONTINUE;


		POP:	//POP & DO NOTHING
			n1 = my_pop();
		CONTINUE;
		
		//push digit
		PUT0: my_push(0); CONTINUE;
		PUT1: my_push(1); CONTINUE;
		PUT2: my_push(2); CONTINUE;
		PUT3: my_push(3); CONTINUE;
		PUT4: my_push(4); CONTINUE;
		PUT5: my_push(5); CONTINUE;
		PUT6: my_push(6); CONTINUE;
		PUT7: my_push(7); CONTINUE;
		PUT8: my_push(8); CONTINUE;
		PUT9: my_push(9); CONTINUE;
		
		
		/* ========== GRID COMMANDS ========== */
		
		GET:	//get command
			n2 = my_pop();
			n1 = my_pop();
			x_pos_ = (int_fast64_t) n1;
			y_pos_ = (int_fast64_t) n2;
			n3 = (int_fast64_t) ins[y_pos_][x_pos_]; //get command
			my_push(n3);
		CONTINUE;
		


		PUT:	//CHANGE GRID
			n3 = my_pop();
			n2 = my_pop();
			n1 = my_pop();
			
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
			n1 = my_pop();
			printf("%d", (int) n1);
			fflush(stdout);
		CONTINUE;


		PR_CHAR:	//PRINT AS CHAR	
			n1 = my_pop();
			printf("%c", (char) n1);
			fflush(stdout);
		CONTINUE;


		G_INT:	//TAKE INT INPUT	
			scanf("%ld", &n1);
			my_push(n1);
		CONTINUE;


		G_CHAR:	//TAKE CHAR INPUT
			scanf("%c", &c);
			my_push((int_fast64_t) c);
		CONTINUE;
		
		
		STR:	//TOGGLE STRING MODE
			string_mode = 1 - string_mode;
		CONTINUE;
	
		
		
		/* ================ HALT ================ */
		
		HALT: 
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
