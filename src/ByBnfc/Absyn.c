/* C Abstract Syntax Implementation generated by the BNF Converter. */

#include <stdio.h>
#include <stdlib.h>
#include "Absyn.h"

/********************   Program    ********************/

Program make_Program(ListTopDef p1)
{
    Program tmp = (Program) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Program!\n");
        exit(1);
    }
    tmp->kind = is_Program;
    tmp->u.program_.listtopdef_ = p1;
    return tmp;
}

/********************   FnDef    ********************/

TopDef make_FnDef(Type p1, Ident p2, ListFnArg p3, Block p4)
{
    TopDef tmp = (TopDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating FnDef!\n");
        exit(1);
    }
    tmp->kind = is_FnDef;
    tmp->u.fndef_.type_ = p1;
    tmp->u.fndef_.ident_ = p2;
    tmp->u.fndef_.listfnarg_ = p3;
    tmp->u.fndef_.block_ = p4;
    return tmp;
}

/********************   ClassDef    ********************/

TopDef make_ClassDef(Ident p1, ListClassMemberDef p2)
{
    TopDef tmp = (TopDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ClassDef!\n");
        exit(1);
    }
    tmp->kind = is_ClassDef;
    tmp->u.classdef_.ident_ = p1;
    tmp->u.classdef_.listclassmemberdef_ = p2;
    return tmp;
}

/********************   ClassDefExtends    ********************/

TopDef make_ClassDefExtends(Ident p1, Ident p2, ListClassMemberDef p3)
{
    TopDef tmp = (TopDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ClassDefExtends!\n");
        exit(1);
    }
    tmp->kind = is_ClassDefExtends;
    tmp->u.classdefextends_.ident_1 = p1;
    tmp->u.classdefextends_.ident_2 = p2;
    tmp->u.classdefextends_.listclassmemberdef_ = p3;
    return tmp;
}

/********************   ListTopDef    ********************/

ListTopDef make_ListTopDef(TopDef p1, ListTopDef p2)
{
    ListTopDef tmp = (ListTopDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListTopDef!\n");
        exit(1);
    }
    tmp->topdef_ = p1;
    tmp->listtopdef_ = p2;
    return tmp;
}

/********************   FieldDecl    ********************/

ClassMemberDef make_FieldDecl(Type p1, ListFieldDeclItem p2)
{
    ClassMemberDef tmp = (ClassMemberDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating FieldDecl!\n");
        exit(1);
    }
    tmp->kind = is_FieldDecl;
    tmp->u.fielddecl_.type_ = p1;
    tmp->u.fielddecl_.listfielddeclitem_ = p2;
    return tmp;
}

/********************   Method    ********************/

ClassMemberDef make_Method(Type p1, Ident p2, ListFnArg p3, Block p4)
{
    ClassMemberDef tmp = (ClassMemberDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Method!\n");
        exit(1);
    }
    tmp->kind = is_Method;
    tmp->u.method_.type_ = p1;
    tmp->u.method_.ident_ = p2;
    tmp->u.method_.listfnarg_ = p3;
    tmp->u.method_.block_ = p4;
    return tmp;
}

/********************   ListClassMemberDef    ********************/

ListClassMemberDef make_ListClassMemberDef(ClassMemberDef p1, ListClassMemberDef p2)
{
    ListClassMemberDef tmp = (ListClassMemberDef) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListClassMemberDef!\n");
        exit(1);
    }
    tmp->classmemberdef_ = p1;
    tmp->listclassmemberdef_ = p2;
    return tmp;
}

/********************   FieldDeclItem    ********************/

FieldDeclItem make_FieldDeclItem(Ident p1)
{
    FieldDeclItem tmp = (FieldDeclItem) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating FieldDeclItem!\n");
        exit(1);
    }
    tmp->kind = is_FieldDeclItem;
    tmp->u.fielddeclitem_.ident_ = p1;
    return tmp;
}

/********************   ListFieldDeclItem    ********************/

ListFieldDeclItem make_ListFieldDeclItem(FieldDeclItem p1, ListFieldDeclItem p2)
{
    ListFieldDeclItem tmp = (ListFieldDeclItem) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListFieldDeclItem!\n");
        exit(1);
    }
    tmp->fielddeclitem_ = p1;
    tmp->listfielddeclitem_ = p2;
    return tmp;
}

/********************   FnArg    ********************/

FnArg make_FnArg(Type p1, Ident p2)
{
    FnArg tmp = (FnArg) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating FnArg!\n");
        exit(1);
    }
    tmp->kind = is_FnArg;
    tmp->u.fnarg_.type_ = p1;
    tmp->u.fnarg_.ident_ = p2;
    return tmp;
}

/********************   ListFnArg    ********************/

ListFnArg make_ListFnArg(FnArg p1, ListFnArg p2)
{
    ListFnArg tmp = (ListFnArg) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListFnArg!\n");
        exit(1);
    }
    tmp->fnarg_ = p1;
    tmp->listfnarg_ = p2;
    return tmp;
}

/********************   Block    ********************/

Block make_Block(ListStmt p1)
{
    Block tmp = (Block) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Block!\n");
        exit(1);
    }
    tmp->kind = is_Block;
    tmp->u.block_.liststmt_ = p1;
    return tmp;
}

/********************   ListStmt    ********************/

ListStmt make_ListStmt(Stmt p1, ListStmt p2)
{
    ListStmt tmp = (ListStmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListStmt!\n");
        exit(1);
    }
    tmp->stmt_ = p1;
    tmp->liststmt_ = p2;
    return tmp;
}

/********************   SEmpty    ********************/

Stmt make_SEmpty()
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SEmpty!\n");
        exit(1);
    }
    tmp->kind = is_SEmpty;
    return tmp;
}

/********************   SBlock    ********************/

Stmt make_SBlock(Block p1)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SBlock!\n");
        exit(1);
    }
    tmp->kind = is_SBlock;
    tmp->u.sblock_.block_ = p1;
    return tmp;
}

/********************   SDecl    ********************/

Stmt make_SDecl(Type p1, ListDeclItem p2)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SDecl!\n");
        exit(1);
    }
    tmp->kind = is_SDecl;
    tmp->u.sdecl_.type_ = p1;
    tmp->u.sdecl_.listdeclitem_ = p2;
    return tmp;
}

/********************   SAss    ********************/

Stmt make_SAss(Expr p1, AssOp p2, Expr p3)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SAss!\n");
        exit(1);
    }
    tmp->kind = is_SAss;
    tmp->u.sass_.expr_1 = p1;
    tmp->u.sass_.assop_ = p2;
    tmp->u.sass_.expr_2 = p3;
    return tmp;
}

/********************   SIncr    ********************/

Stmt make_SIncr(Expr p1, IncrOp p2)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SIncr!\n");
        exit(1);
    }
    tmp->kind = is_SIncr;
    tmp->u.sincr_.expr_ = p1;
    tmp->u.sincr_.incrop_ = p2;
    return tmp;
}

/********************   SDecr    ********************/

Stmt make_SDecr(Expr p1, DecrOp p2)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SDecr!\n");
        exit(1);
    }
    tmp->kind = is_SDecr;
    tmp->u.sdecr_.expr_ = p1;
    tmp->u.sdecr_.decrop_ = p2;
    return tmp;
}

/********************   SRet    ********************/

Stmt make_SRet(Expr p1)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SRet!\n");
        exit(1);
    }
    tmp->kind = is_SRet;
    tmp->u.sret_.expr_ = p1;
    return tmp;
}

/********************   SVRet    ********************/

Stmt make_SVRet()
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SVRet!\n");
        exit(1);
    }
    tmp->kind = is_SVRet;
    return tmp;
}

/********************   SExpr    ********************/

Stmt make_SExpr(Expr p1)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SExpr!\n");
        exit(1);
    }
    tmp->kind = is_SExpr;
    tmp->u.sexpr_.expr_ = p1;
    return tmp;
}

/********************   SWhile    ********************/

Stmt make_SWhile(Expr p1, Stmt p2)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SWhile!\n");
        exit(1);
    }
    tmp->kind = is_SWhile;
    tmp->u.swhile_.expr_ = p1;
    tmp->u.swhile_.stmt_ = p2;
    return tmp;
}

/********************   SFor    ********************/

Stmt make_SFor(Type p1, Ident p2, Expr p3, Stmt p4)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SFor!\n");
        exit(1);
    }
    tmp->kind = is_SFor;
    tmp->u.sfor_.type_ = p1;
    tmp->u.sfor_.ident_ = p2;
    tmp->u.sfor_.expr_ = p3;
    tmp->u.sfor_.stmt_ = p4;
    return tmp;
}

/********************   SIf    ********************/

Stmt make_SIf(Expr p1, Stmt p2)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SIf!\n");
        exit(1);
    }
    tmp->kind = is_SIf;
    tmp->u.sif_.expr_ = p1;
    tmp->u.sif_.stmt_ = p2;
    return tmp;
}

/********************   SIfElse    ********************/

Stmt make_SIfElse(Expr p1, Stmt p2, Stmt p3)
{
    Stmt tmp = (Stmt) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating SIfElse!\n");
        exit(1);
    }
    tmp->kind = is_SIfElse;
    tmp->u.sifelse_.expr_ = p1;
    tmp->u.sifelse_.stmt_1 = p2;
    tmp->u.sifelse_.stmt_2 = p3;
    return tmp;
}

/********************   DNoInit    ********************/

DeclItem make_DNoInit(Ident p1)
{
    DeclItem tmp = (DeclItem) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating DNoInit!\n");
        exit(1);
    }
    tmp->kind = is_DNoInit;
    tmp->u.dnoinit_.ident_ = p1;
    return tmp;
}

/********************   DInit    ********************/

DeclItem make_DInit(Ident p1, AssOp p2, Expr p3)
{
    DeclItem tmp = (DeclItem) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating DInit!\n");
        exit(1);
    }
    tmp->kind = is_DInit;
    tmp->u.dinit_.ident_ = p1;
    tmp->u.dinit_.assop_ = p2;
    tmp->u.dinit_.expr_ = p3;
    return tmp;
}

/********************   ListDeclItem    ********************/

ListDeclItem make_ListDeclItem(DeclItem p1, ListDeclItem p2)
{
    ListDeclItem tmp = (ListDeclItem) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListDeclItem!\n");
        exit(1);
    }
    tmp->declitem_ = p1;
    tmp->listdeclitem_ = p2;
    return tmp;
}

/********************   EVar    ********************/

Expr make_EVar(Ident p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EVar!\n");
        exit(1);
    }
    tmp->kind = is_EVar;
    tmp->u.evar_.ident_ = p1;
    return tmp;
}

/********************   ELitInt    ********************/

Expr make_ELitInt(IntLiteral p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ELitInt!\n");
        exit(1);
    }
    tmp->kind = is_ELitInt;
    tmp->u.elitint_.intliteral_ = p1;
    return tmp;
}

/********************   ELitTrue    ********************/

Expr make_ELitTrue()
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ELitTrue!\n");
        exit(1);
    }
    tmp->kind = is_ELitTrue;
    return tmp;
}

/********************   ELitFalse    ********************/

Expr make_ELitFalse()
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ELitFalse!\n");
        exit(1);
    }
    tmp->kind = is_ELitFalse;
    return tmp;
}

/********************   ESelf    ********************/

Expr make_ESelf()
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ESelf!\n");
        exit(1);
    }
    tmp->kind = is_ESelf;
    return tmp;
}

/********************   ENull    ********************/

Expr make_ENull()
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ENull!\n");
        exit(1);
    }
    tmp->kind = is_ENull;
    return tmp;
}

/********************   ECastedNull    ********************/

Expr make_ECastedNull(Expr p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ECastedNull!\n");
        exit(1);
    }
    tmp->kind = is_ECastedNull;
    tmp->u.ecastednull_.expr_ = p1;
    return tmp;
}

/********************   ECastedArrNull    ********************/

Expr make_ECastedArrNull(Type p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ECastedArrNull!\n");
        exit(1);
    }
    tmp->kind = is_ECastedArrNull;
    tmp->u.ecastedarrnull_.type_ = p1;
    return tmp;
}

/********************   ELitStr    ********************/

Expr make_ELitStr(String p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ELitStr!\n");
        exit(1);
    }
    tmp->kind = is_ELitStr;
    tmp->u.elitstr_.string_ = p1;
    return tmp;
}

/********************   EArrElem    ********************/

Expr make_EArrElem(Expr p1, LBracket p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EArrElem!\n");
        exit(1);
    }
    tmp->kind = is_EArrElem;
    tmp->u.earrelem_.expr_1 = p1;
    tmp->u.earrelem_.lbracket_ = p2;
    tmp->u.earrelem_.expr_2 = p3;
    return tmp;
}

/********************   ECallFunc    ********************/

Expr make_ECallFunc(Ident p1, LParen p2, ListExpr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ECallFunc!\n");
        exit(1);
    }
    tmp->kind = is_ECallFunc;
    tmp->u.ecallfunc_.ident_ = p1;
    tmp->u.ecallfunc_.lparen_ = p2;
    tmp->u.ecallfunc_.listexpr_ = p3;
    return tmp;
}

/********************   EField    ********************/

Expr make_EField(Expr p1, Dot p2, Ident p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EField!\n");
        exit(1);
    }
    tmp->kind = is_EField;
    tmp->u.efield_.expr_ = p1;
    tmp->u.efield_.dot_ = p2;
    tmp->u.efield_.ident_ = p3;
    return tmp;
}

/********************   ECallMethod    ********************/

Expr make_ECallMethod(Expr p1, Dot p2, Ident p3, LParen p4, ListExpr p5)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ECallMethod!\n");
        exit(1);
    }
    tmp->kind = is_ECallMethod;
    tmp->u.ecallmethod_.expr_ = p1;
    tmp->u.ecallmethod_.dot_ = p2;
    tmp->u.ecallmethod_.ident_ = p3;
    tmp->u.ecallmethod_.lparen_ = p4;
    tmp->u.ecallmethod_.listexpr_ = p5;
    return tmp;
}

/********************   ENewArray    ********************/

Expr make_ENewArray(Type p1, Expr p2)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ENewArray!\n");
        exit(1);
    }
    tmp->kind = is_ENewArray;
    tmp->u.enewarray_.type_ = p1;
    tmp->u.enewarray_.expr_ = p2;
    return tmp;
}

/********************   ENewClass    ********************/

Expr make_ENewClass(Ident p1)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ENewClass!\n");
        exit(1);
    }
    tmp->kind = is_ENewClass;
    tmp->u.enewclass_.ident_ = p1;
    return tmp;
}

/********************   ENeg    ********************/

Expr make_ENeg(NegOp p1, Expr p2)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ENeg!\n");
        exit(1);
    }
    tmp->kind = is_ENeg;
    tmp->u.eneg_.negop_ = p1;
    tmp->u.eneg_.expr_ = p2;
    return tmp;
}

/********************   ENot    ********************/

Expr make_ENot(NotOp p1, Expr p2)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ENot!\n");
        exit(1);
    }
    tmp->kind = is_ENot;
    tmp->u.enot_.notop_ = p1;
    tmp->u.enot_.expr_ = p2;
    return tmp;
}

/********************   EMul    ********************/

Expr make_EMul(Expr p1, MulOp p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EMul!\n");
        exit(1);
    }
    tmp->kind = is_EMul;
    tmp->u.emul_.expr_1 = p1;
    tmp->u.emul_.mulop_ = p2;
    tmp->u.emul_.expr_2 = p3;
    return tmp;
}

/********************   EAdd    ********************/

Expr make_EAdd(Expr p1, AddOp p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EAdd!\n");
        exit(1);
    }
    tmp->kind = is_EAdd;
    tmp->u.eadd_.expr_1 = p1;
    tmp->u.eadd_.addop_ = p2;
    tmp->u.eadd_.expr_2 = p3;
    return tmp;
}

/********************   ERel    ********************/

Expr make_ERel(Expr p1, RelOp p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ERel!\n");
        exit(1);
    }
    tmp->kind = is_ERel;
    tmp->u.erel_.expr_1 = p1;
    tmp->u.erel_.relop_ = p2;
    tmp->u.erel_.expr_2 = p3;
    return tmp;
}

/********************   EAnd    ********************/

Expr make_EAnd(Expr p1, AndOp p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EAnd!\n");
        exit(1);
    }
    tmp->kind = is_EAnd;
    tmp->u.eand_.expr_1 = p1;
    tmp->u.eand_.andop_ = p2;
    tmp->u.eand_.expr_2 = p3;
    return tmp;
}

/********************   EOr    ********************/

Expr make_EOr(Expr p1, OrOp p2, Expr p3)
{
    Expr tmp = (Expr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EOr!\n");
        exit(1);
    }
    tmp->kind = is_EOr;
    tmp->u.eor_.expr_1 = p1;
    tmp->u.eor_.orop_ = p2;
    tmp->u.eor_.expr_2 = p3;
    return tmp;
}

/********************   ListExpr    ********************/

ListExpr make_ListExpr(Expr p1, ListExpr p2)
{
    ListExpr tmp = (ListExpr) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating ListExpr!\n");
        exit(1);
    }
    tmp->expr_ = p1;
    tmp->listexpr_ = p2;
    return tmp;
}

/********************   TInt    ********************/

Type make_TInt()
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TInt!\n");
        exit(1);
    }
    tmp->kind = is_TInt;
    return tmp;
}

/********************   TStr    ********************/

Type make_TStr()
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TStr!\n");
        exit(1);
    }
    tmp->kind = is_TStr;
    return tmp;
}

/********************   TBool    ********************/

Type make_TBool()
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TBool!\n");
        exit(1);
    }
    tmp->kind = is_TBool;
    return tmp;
}

/********************   TVoid    ********************/

Type make_TVoid()
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TVoid!\n");
        exit(1);
    }
    tmp->kind = is_TVoid;
    return tmp;
}

/********************   TArray    ********************/

Type make_TArray(Type p1)
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TArray!\n");
        exit(1);
    }
    tmp->kind = is_TArray;
    tmp->u.tarray_.type_ = p1;
    return tmp;
}

/********************   TClass    ********************/

Type make_TClass(Ident p1)
{
    Type tmp = (Type) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating TClass!\n");
        exit(1);
    }
    tmp->kind = is_TClass;
    tmp->u.tclass_.ident_ = p1;
    return tmp;
}

/********************   AssOp    ********************/

AssOp make_AssOp()
{
    AssOp tmp = (AssOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating AssOp!\n");
        exit(1);
    }
    tmp->kind = is_AssOp;
    return tmp;
}

/********************   IncrOp    ********************/

IncrOp make_IncrOp()
{
    IncrOp tmp = (IncrOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating IncrOp!\n");
        exit(1);
    }
    tmp->kind = is_IncrOp;
    return tmp;
}

/********************   DecrOp    ********************/

DecrOp make_DecrOp()
{
    DecrOp tmp = (DecrOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating DecrOp!\n");
        exit(1);
    }
    tmp->kind = is_DecrOp;
    return tmp;
}

/********************   LBracket    ********************/

LBracket make_LBracket()
{
    LBracket tmp = (LBracket) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating LBracket!\n");
        exit(1);
    }
    tmp->kind = is_LBracket;
    return tmp;
}

/********************   LParen    ********************/

LParen make_LParen()
{
    LParen tmp = (LParen) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating LParen!\n");
        exit(1);
    }
    tmp->kind = is_LParen;
    return tmp;
}

/********************   Dot    ********************/

Dot make_Dot()
{
    Dot tmp = (Dot) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Dot!\n");
        exit(1);
    }
    tmp->kind = is_Dot;
    return tmp;
}

/********************   NegOp    ********************/

NegOp make_NegOp()
{
    NegOp tmp = (NegOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating NegOp!\n");
        exit(1);
    }
    tmp->kind = is_NegOp;
    return tmp;
}

/********************   NotOp    ********************/

NotOp make_NotOp()
{
    NotOp tmp = (NotOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating NotOp!\n");
        exit(1);
    }
    tmp->kind = is_NotOp;
    return tmp;
}

/********************   AndOp    ********************/

AndOp make_AndOp()
{
    AndOp tmp = (AndOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating AndOp!\n");
        exit(1);
    }
    tmp->kind = is_AndOp;
    return tmp;
}

/********************   OrOp    ********************/

OrOp make_OrOp()
{
    OrOp tmp = (OrOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating OrOp!\n");
        exit(1);
    }
    tmp->kind = is_OrOp;
    return tmp;
}

/********************   Plus    ********************/

AddOp make_Plus()
{
    AddOp tmp = (AddOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Plus!\n");
        exit(1);
    }
    tmp->kind = is_Plus;
    return tmp;
}

/********************   Minus    ********************/

AddOp make_Minus()
{
    AddOp tmp = (AddOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Minus!\n");
        exit(1);
    }
    tmp->kind = is_Minus;
    return tmp;
}

/********************   Times    ********************/

MulOp make_Times()
{
    MulOp tmp = (MulOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Times!\n");
        exit(1);
    }
    tmp->kind = is_Times;
    return tmp;
}

/********************   Div    ********************/

MulOp make_Div()
{
    MulOp tmp = (MulOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Div!\n");
        exit(1);
    }
    tmp->kind = is_Div;
    return tmp;
}

/********************   Mod    ********************/

MulOp make_Mod()
{
    MulOp tmp = (MulOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating Mod!\n");
        exit(1);
    }
    tmp->kind = is_Mod;
    return tmp;
}

/********************   LTH    ********************/

RelOp make_LTH()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating LTH!\n");
        exit(1);
    }
    tmp->kind = is_LTH;
    return tmp;
}

/********************   LE    ********************/

RelOp make_LE()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating LE!\n");
        exit(1);
    }
    tmp->kind = is_LE;
    return tmp;
}

/********************   GTH    ********************/

RelOp make_GTH()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating GTH!\n");
        exit(1);
    }
    tmp->kind = is_GTH;
    return tmp;
}

/********************   GE    ********************/

RelOp make_GE()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating GE!\n");
        exit(1);
    }
    tmp->kind = is_GE;
    return tmp;
}

/********************   EQU    ********************/

RelOp make_EQU()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating EQU!\n");
        exit(1);
    }
    tmp->kind = is_EQU;
    return tmp;
}

/********************   NE    ********************/

RelOp make_NE()
{
    RelOp tmp = (RelOp) malloc(sizeof(*tmp));
    if (!tmp)
    {
        fprintf(stderr, "Error: out of memory when allocating NE!\n");
        exit(1);
    }
    tmp->kind = is_NE;
    return tmp;
}
