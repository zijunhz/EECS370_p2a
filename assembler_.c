/**
 * Project 1
 * Assembler code fragment for LC-2K
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

//Every LC2K file will contain less than 1000 lines of assembly.
#define MAXLINELENGTH 1000
#define MAXMEMO 65536

int count = 0; //line counts
int datanum=0; //num of datas
int symbolnum=0; //num of symbols
int relocnum=0; //num of relcoctions
//num of text is count+1-datanum!

int readAndParse(FILE *, char *, char *, char *, char *, char *);

static inline int isNumber(char *);

struct command
{
    char label[MAXLINELENGTH];
    char opcode[MAXLINELENGTH];
    char arg0[MAXLINELENGTH];
    char arg1[MAXLINELENGTH];
    char arg2[MAXLINELENGTH];
    int line_address;
};

struct symbol{
    char label[MAXLINELENGTH];
    char status;
    int offset;
};

struct relocation{
    int offset;
    char opcode[MAXLINELENGTH];
    char label[MAXLINELENGTH];
};

struct symbol *symbol_table;
struct relocation *relocation_table;

//checking function sets
int check_dup_labels(struct command *command, int count)
{
    for (int i = 0; i <= count; i++)
    {
      if(strcmp(command[i].label,""))  
      {for (int j = count; j > i; j--)
        {
            if (!strcmp(command[i].label, command[j].label))
                exit(1);
        }}
    }
    return 1;
}
int check_register(char *reg)
{
    if (!isNumber(reg))
    {
        exit(1);
    }
    else if (atoi(reg) < 0 || atoi(reg) > 7)
    {
        exit(1);
    }
    else
    {
        return 1;
    }
}
int search_label(struct command *command, char *symbolic)
{
    for (int i = 0; i <= count; i++)
    {
        if (!strcmp(command[i].label, symbolic))
        {
            return i;
        }
    }
    if(symbolic[0]>='A' && symbolic[0]<='Z'){
        return -1;
    }
    exit(1);
}
int check_offset(int offset)
{
    if ((offset < -32768) || (offset > 32767))
    {
        exit(1);
    }
    return 1;
}

//transformation function
int trans(struct command assembly, struct command *all_assembly)
{
    if (!strcmp(assembly.opcode, "add") || (!strcmp(assembly.opcode, "nor")))
    {
        if (check_register(assembly.arg0) && check_register(assembly.arg1) && check_register(assembly.arg2))
        {
            int a = atoi(assembly.arg0);
            int b = atoi(assembly.arg1);
            int dest = atoi(assembly.arg2);
            if (!strcmp(assembly.opcode, "add"))
                return (((0b000 << 3 | a) << 3 | b) << 16 | dest);
            else
                return (((0b001 << 3 | a) << 3 | b) << 16 | dest);
        }
    }
    else if (!strcmp(assembly.opcode, "lw") 
            || (!strcmp(assembly.opcode, "sw")) 
            || (!strcmp(assembly.opcode, "beq")))
    {
        if (check_register(assembly.arg0) && check_register(assembly.arg1))
        {
            int a = atoi(assembly.arg0);
            int b = atoi(assembly.arg1);
            int offset = 0;
            if (isNumber(assembly.arg2) == 1)
            {
                offset = atoi(assembly.arg2);
            }
            else
            {
                int index = search_label(all_assembly, assembly.arg2);
                if(index==-1){
                    offset=0;
                    symbol_table[symbolnum].offset=0;
                    strcpy(symbol_table[symbolnum].label, assembly.arg2);
                    symbol_table[symbolnum].status='U';
                    symbolnum++;
                }else{
                    offset = all_assembly[index].line_address;
                    if(assembly.arg2[0]>='A'&&assembly.arg2[0]<='Z'){
                        symbol_table[symbolnum].offset=offset;
                        strcpy(symbol_table[symbolnum].label, assembly.arg2);
                        symbol_table[symbolnum].status='T';
                        symbolnum++;
                    }
                }
            }
            if (check_offset(offset))
            {
                if (!strcmp(assembly.opcode, "lw"))
                {   
                    if (isNumber(assembly.arg2) == 0){
                    relocation_table[relocnum].offset=assembly.line_address;
                    strcpy(relocation_table[relocnum].opcode, "lw");
                    strcpy(relocation_table[relocnum].label, assembly.arg2);
                    relocnum++;
                    }
                    if (offset < 0)
                        return (((0b010 << 3 | a) << 3 | b) << 16 | (offset & 65535));
                    else
                        return (((0b010 << 3 | a) << 3 | b) << 16 | offset);
                }
                else if (!strcmp(assembly.opcode, "sw"))
                {
                    if (isNumber(assembly.arg2) == 0){
                    relocation_table[relocnum].offset=assembly.line_address;
                    strcpy(relocation_table[relocnum].opcode, "sw");
                    strcpy(relocation_table[relocnum].label, assembly.arg2);
                    relocnum++;
                    }
                    if (offset < 0)
                        return (((0b011 << 3 | a) << 3 | b) << 16 | (offset & 65535));
                    else
                        return (((0b011 << 3 | a) << 3 | b) << 16 | offset);
                }
                else
                {   
                    if(search_label(all_assembly, assembly.arg2)==-1){
                        //undefined global in beq
                        exit(1);
                    }
                    if(!isNumber(assembly.arg2))offset = offset - 1 - assembly.line_address;
                    if (offset < 0)
                        return (((0b100 << 3 | a) << 3 | b) << 16 | (offset & 65535));
                    else
                        return (((0b100 << 3 | a) << 3 | b) << 16 | offset);
                }
            }
        }
    }
    else if (!strcmp(assembly.opcode, "jalr"))
    {
        if (check_register(assembly.arg0) && check_register(assembly.arg1))
        {
            int a = atoi(assembly.arg0);
            int b = atoi(assembly.arg1);
            return (((0b101 << 3 | a) << 3 | b) << 16);
        }
    }
    else if (!strcmp(assembly.opcode, "halt"))
    {
        return (0b110 << 22);
    }
    else if (!strcmp(assembly.opcode, "noop"))
    {
        return (0b111 << 22);
    }
    else if (!strcmp(assembly.opcode, ".fill"))
    {   
        datanum++;
        if (isNumber(assembly.arg0))
        {
            return atoi(assembly.arg0);
        }
        else
        {
            relocation_table[relocnum].offset=datanum-1;
            strcpy(relocation_table[relocnum].opcode, ".fill");
            strcpy(relocation_table[relocnum].label, assembly.arg0);
            relocnum++;
            int index = search_label(all_assembly, assembly.arg0);
            if(index==-1){
                    if(assembly.label[0]>='A'&&assembly.label[0]<='Z'){
                    symbol_table[symbolnum].offset=0;
                    strcpy(symbol_table[symbolnum].label, assembly.label);
                    symbol_table[symbolnum].status='D';
                    symbolnum++;
                    }       
                    symbol_table[symbolnum].offset=0;
                    strcpy(symbol_table[symbolnum].label, assembly.arg0);
                    symbol_table[symbolnum].status='U';
                    symbolnum++;
                    return 0;
            }
            else{
                if(assembly.label[0]>='A'&&assembly.label[0]<='Z'){
                    symbol_table[symbolnum].offset=datanum-1;
                    strcpy(symbol_table[symbolnum].label, assembly.label);
                    symbol_table[symbolnum].status='D';
                    symbolnum++;
                    }      
                return all_assembly[index].line_address;
            }
        }
    }
    else
    {
        exit(1);
    }
    return 0;//unrecognized opcode
}

int main(int argc, char **argv)
{   
    //int argc = 3;
    //char *argv[] = {"program_name", "test_all.as", "test_all.mc"};
    char *inFileString, *outFileString;
    FILE *inFilePtr, *outFilePtr;
    char label[MAXLINELENGTH], opcode[MAXLINELENGTH], arg0[MAXLINELENGTH],
        arg1[MAXLINELENGTH], arg2[MAXLINELENGTH];
    int machine[65536] = {0};
    int header[4];
    struct command *assembly_code = (struct command *)malloc(sizeof(struct command) * MAXMEMO);
    symbol_table=(struct symbol *)malloc(sizeof(struct symbol) * MAXMEMO);
    relocation_table= (struct relocation *)malloc(sizeof(struct relocation) * MAXMEMO);
    if (argc != 3)
    {
        printf("error: usage: %s <assembly-code-file> <machine-code-file>\n",
               argv[0]);
        exit(1);
    }

    inFileString = argv[1];
    outFileString = argv[2];

    inFilePtr = fopen(inFileString, "r");
    if (inFilePtr == NULL)
    {
        printf("error in opening %s\n", inFileString);
        exit(1);
    }
    outFilePtr = fopen(outFileString, "w");
    if (outFilePtr == NULL)
    {
        printf("error in opening %s\n", outFileString);
        exit(1);
    }

    while (readAndParse(inFilePtr, label, opcode, arg0, arg1, arg2))
    {
        strcpy(assembly_code[count].label, label);
        strcpy(assembly_code[count].opcode, opcode);
        strcpy(assembly_code[count].arg0, arg0);
        strcpy( assembly_code[count].arg1,arg1);
        strcpy( assembly_code[count].arg2,arg2);
        assembly_code[count].line_address = count;
        count++;
        
    }
    
    if (!check_dup_labels(assembly_code, count))
    {
        exit(1);
    }
    for (int i = 0; i <count; i++)
    {
        machine[i] = trans(assembly_code[i], assembly_code);
    }
    header[0]=count-datanum;//t
    header[1]=datanum;//d
    header[2]=symbolnum;
    header[3]=relocnum;
    //print result:
    //header:
    fprintf(outFilePtr, "%d%c%d%c%d%c%d\n",
    header[0],' ',header[1],' ',header[2],' ',header[3]);
    //text and data:
    for (int i = 0; i <count; i++)
    {
        fprintf(outFilePtr, "%d\n", machine[i]);
    }
    //symbol table:
    for(int i=0;i<symbolnum;i++){
        fprintf(outFilePtr, "%s%c%c%c%d\n", 
        symbol_table[i].label,' ',symbol_table[i].status,' ',symbol_table[i].offset);
    }
    //relocation table:
    for(int i=0;i<relocnum;i++){
        fprintf(outFilePtr, "%d%c%s%c%s\n", 
        relocation_table[i].offset,' ',relocation_table[i].opcode,' ',relocation_table[i].label);
    }
    fclose(inFilePtr);
    fclose(outFilePtr);
    free(assembly_code);
    free(relocation_table);
    free(symbol_table);
    return 0;
}

/*
* NOTE: The code defined below is not to be modifed as it is implimented correctly.
*/

/*
 * Read and parse a line of the assembly-language file.  Fields are returned
 * in label, opcode, arg0, arg1, arg2 (these strings must have memory already
 * allocated to them).
 *
 * Return values:
 *     0 if reached end of file
 *     1 if all went well
 *
 * exit(1) if line is too long.
 */
int readAndParse(FILE *inFilePtr, char *label, char *opcode, char *arg0,
                 char *arg1, char *arg2)
{
    char line[MAXLINELENGTH];
    char *ptr = line;

    /* delete prior values */
    label[0] = opcode[0] = arg0[0] = arg1[0] = arg2[0] = '\0';

    /* read the line from the assembly-language file */
    if (fgets(line, MAXLINELENGTH, inFilePtr) == NULL)
    {
        /* reached end of file */
        return (0);
    }

    /* check for line too long */
    if (strlen(line) == MAXLINELENGTH - 1)
    {
        printf("error: line too long\n");
        exit(1);
    }

    // Treat a blank line as end of file.
    // Arguably, we could just ignore and continue, but that could
    // get messy in terms of label line numbers etc.
    char whitespace[4] = {'\t', '\n', '\r', ' '};
    int nonempty_line = 0;
    for (size_t line_idx = 0; line_idx < strlen(line); ++line_idx)
    {
        int line_char_is_whitespace = 0;
        for (int whitespace_idx = 0; whitespace_idx < 4; ++whitespace_idx)
        {
            if (line[line_idx] == whitespace[whitespace_idx])
            {
                ++line_char_is_whitespace;
                break;
            }
        }
        if (!line_char_is_whitespace)
        {
            ++nonempty_line;
            break;
        }
    }
    if (nonempty_line == 0)
    {
        return 0;
    }

    /* is there a label? */
    ptr = line;
    if (sscanf(ptr, "%[^\t\n ]", label))
    {
        /* successfully read label; advance pointer over the label */
        ptr += strlen(label);
    }

    /*
     * Parse the rest of the line.  Would be nice to have real regular
     * expressions, but scanf will suffice.
     */
    sscanf(ptr, "%*[\t\n\r ]%[^\t\n\r ]%*[\t\n\r ]%[^\t\n\r ]%*[\t\n\r ]%[^\t\n\r ]%*[\t\n\r ]%[^\t\n\r ]",
           opcode, arg0, arg1, arg2);

    return (1);
}

static inline int
isNumber(char *string)
{
    int num;
    char c;
    return ((sscanf(string, "%d%c", &num, &c)) == 1);
}
