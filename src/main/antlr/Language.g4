grammar Language;

@header {
    package tvestergaard.compiler.antlr;
}

input
    :   statementUse*
        declaration*
        statement*
        EOF
    ;

/*******************************************************************************


    Use statements.


*******************************************************************************/

statementUse
    :   USE
        statementUseName
        statementUseRename?
        SEMICOLON
    ;

statementUseName
    :   IDENTIFIER (DOUBLECOLON statementUseNameSection)*
    ;

statementUseRename
    :   AS name
    ;

statementUseNameSection
    :   PERIOD
    |   IDENTIFIER
    |   statementUseCommands
    |   statementUseCommandType
    ;

statementUseCommands
    :   CURLY_OPEN
        statementUseCommand? (COMMA statementUseCommand)* COMMA
        CURLY_CLOSE
    ;

statementUseCommand
    :   statementUseCommandSub
    |   statementUseCommandType
    |   statementUseCommandMatch
    ;

statementUseCommandSub
    :   name statementUseRename?
    ;

statementUseCommandMatch
    :   PERIOD? name PERIOD
    |   PERIOD  name PERIOD?
    ;

statementUseCommandType
    :   MODULE
    |   CLASS
    |   INTERFACE
    |   TRAIT
    |   ENUMERATION
    |   STRUCTURE
    |   FUNCTION
    |   CONSTANT
    |   VARIABLE
    |   TYPE
    ;

/*******************************************************************************


    Declarations


*******************************************************************************/

declaration
    :   declarationModule
    |   declarationClass
    |   declarationInterface
    |   declarationTrait
    |   declarationEnumeration
    |   declarationStructure
    |   declarationFunction
    |   declarationConstant
    |   declarationVariable
    |   declarationType
    |   declarationTest
    |   declarationExtension
    ;

/*  Module declarations */

declarationModule
    :   declarationModuleAttribute*
        MODULE name
        typeGuards?
        CURLY_OPEN
        declaration*
        CURLY_CLOSE
    ;

declarationModuleAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    ;

/*  Class declaration */

declarationClass
    :   declarationClassAttribute*
        CLASS name
        genericParameters?
        typeGuards?
        CURLY_OPEN
        declarationClassMember*
        CURLY_CLOSE
    ;

declarationClassAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationClassMember
    :   declarationConstructor
    |   declarationMethod
    |   declarationField
    |   declarationProperty
    |   declarationInvariant
    |   declarationDelegator
    |   declaration
    ;

/* Interface declaration */

declarationInterface
    :   declarationInterfaceAttribute*
        INTERFACE name
        genericParameters?
        typeGuards?
        CURLY_OPEN
        declarationInterfaceMember*
        CURLY_CLOSE
    ;

declarationInterfaceAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationInterfaceMember
    :   declarationMethod
    |   declarationProperty
    |   declarationInvariant
    |   declaration
    ;

/* Trait declaration */

declarationTrait
    :   declarationTraitAttribute*
        TRAIT name
        genericParameters?
        typeGuards?
        CURLY_OPEN
        declarationTraitMember*
        CURLY_CLOSE
    ;

declarationTraitAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationTraitMember
    :   declarationMethod
    |   declarationField
    |   declarationProperty
    |   declarationInvariant
    |   declaration
    ;

/* Enumerator declaration */

declarationEnumeration
    :   declarationEnumerationAttribute*
        ENUMERATION name
        typeGuard?
        genericParameters?
        typeGuards?
        CURLY_OPEN
        declarationEnumerationMember*
        CURLY_CLOSE
    ;

declarationEnumerationAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationEnumerationMember
    :   declarationEnumerationConstant
    |   declarationConstructor
    |   declarationMethod
    |   declarationField
    |   declarationProperty
    |   declarationInvariant
    |   declaration
    ;

declarationEnumerationConstant
    :   CASE IDENTIFIER typeGuard?
        (ASSIGN expression)?
        SEMICOLON
    ;

/* Structure declaration */

declarationStructure
    :   declarationStructureAttribute*
        STRUCTURE name
        genericParameters?
        typeGuards?
        CURLY_OPEN
        declarationStructureMember*
        CURLY_CLOSE
    ;

declarationStructureAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    ;

declarationStructureMember
    :   declarationConstructor
    |   declarationField
    |   declaration
    ;

/* Function declaration */

declarationFunction
    :   declarationFunctionAttribute*
        FUNCTION name
        genericParameters?
        parameters?
        typeGuard?
        declarationFunctionContents
    ;

declarationFunctionAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   PURE
    |   FINAL
    |   INLINE
    |   UNSAFE
    ;

declarationFunctionContents
    :   codeConditional
    |   codeStatement
    |   SEMICOLON
    ;

/* Constant declaration */

declarationConstant
    :   declarationConstantAttribute*
        CONSTANT name
        typeGuard?
        (ASSIGN expression)?
        SEMICOLON
    ;

declarationConstantAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    ;

/* Variable declaration */

declarationVariable
    :   declarationVariableAttribute*
        VARIABLE name
        typeGuard?
        (ASSIGN expression)?
    ;

declarationVariableAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    ;

/* Custom type declaration */

declarationType
    :   declarationTypeAttribute*
        TYPE name
        genericParameters?
        ASSIGN type
        SEMICOLON
    ;

declarationTypeAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    ;

/* Test declaration */

declarationTest
    :   declarationTestAttribute
        TEST name
        declarationTestContents
    ;

declarationTestAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   PURE
    |   FINAL
    |   INLINE
    |   UNSAFE
    ;

declarationTestContents
    :   codeConditional
    |   codeStatement
    |   SEMICOLON
    ;

/* Extension declaration */

declarationExtension
    :   EXTEND declaration
    ;

/*******************************************************************************


    Type member rules


*******************************************************************************/

/* Method declaration */

declarationMethod
    :   declarationMethodAttribute*
        IDENTIFIER
        genericParameters?
        parameters
        typeGuard?
        declarationMethodContents
    ;

declarationMethodAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   PURE
    |   INLINE
    |   UNSAFE
    ;

declarationMethodContents
    :   codeConditional
    |   codeStatement
    |   SEMICOLON
    ;

/* Constructor declaration */

declarationConstructor
    :   declarationConstructorAttribute*
        CONSTRUCTOR
        genericParameters?
        parameters
        declarationConstructorContents
    ;

declarationConstructorAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   PURE
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationConstructorContents
    :   codeConditional
    |   codeStatement
    |   SEMICOLON
    ;

/* Field declaration */

declarationField
    :   declarationFieldAttribute*
        IDENTIFIER typeGuard?
        (ASSIGN expression)?
        declarationFieldProperties
    ;

declarationFieldAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   DELEGATE
    |   DYNAMIC
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationFieldProperties
    :   (CURLY_OPEN
        declarationFieldProperty*
        CURLY_CLOSE)
    |   SEMICOLON
    ;

declarationFieldProperty
    :   declarationPropertyAttribute*
        (GET|SET)
        declarationFieldPropertyContents
    ;

declarationFieldPropertyContents
    :   codeConditional
    |   codeStatement
    |   SEMICOLON
    ;

/* Property declaration */

declarationProperty
    :   declarationPropertyAttribute*
        (GET|SET)
        IDENTIFIER
        typeGuard
        declarationPropertyContents
    ;

declarationPropertyAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   FINAL
    |   LAZY
    |   MUTABLE
    |   IMMUTABLE
    |   UNSAFE
    ;

declarationPropertyContents
    :   codeConditional
    |   codeBlockOrStatement
    |   SEMICOLON
    ;

/* Invariant declaration */

declarationInvariant
    :   declarationInvariantAttribute*
        INVARIANT IDENTIFIER
        declarationInvariantContents
    ;

declarationInvariantContents
    :   codeBlockOrStatement
    |   codeBlock
    ;

declarationInvariantAttribute
    :   MUTABLE
    |   IMMUTABLE
    ;

/* Delegator declaration */

declarationDelegator
    :   declarationDelegatorAttribute*
        DELEGATE SELF declarationDelegatorElement+ (AS IDENTIFIER)?
        SEMICOLON
    ;

declarationDelegatorAttribute
    :   PUBLIC
    |   PRIVATE
    |   PROTECTED
    |   MUTABLE
    |   IMMUTABLE
    ;

declarationDelegatorElement
    :   expressionAccessArray
    |   expressionAccessMethod
    |   expressionAccessProperty
    ;

/* Conditional declaration */

declarationPrecondition
    :   REQUIRE IDENTIFIER
        codeBlockOrStatement
    ;

declarationPostcondition
    :   ENSURE IDENTIFIER
        codeBlockOrStatement
    ;

/*******************************************************************************


    Identifiers, names and types


*******************************************************************************/

name
    :   IDENTIFIER (DOUBLECOLON IDENTIFIER)*
    ;

type
    :   typeUnit
    |   typeUnion
    |   typeIntersection
    ;

/* Type guards */

typeGuard
    :   COLON typeGuardType
    ;

typeGuards
    :   (COLON typeGuardType)+
    ;

typeGuardType
    :   type
    |   SELF
    |   MUTABLE
    |   IMMUTABLE
    ;

/* Compound types */

typeUnion
    :   typeUnit (BITWISE_OR typeUnit)+
    ;

typeIntersection
    :   typeUnit (BITWISE_AND typeUnit)+
    ;

/* The smallest type */

typeUnit
    :   typeAttribute? typeInternal QUESTION?
    |   typeAttribute? typeExternal QUESTION?
    ;

typeAttribute
    :   MUTABLE
    |   IMMUTABLE
    ;

/* Internal unit types */

typeInternal
    :   typeMagic
    |   typePointer
    |   typeTuple
    |   typeArray
    |   typeDictionary
    |   typeCallable
    |   typePrimitive
    ;

typeMagic
    :   TYPE
    ;

typePointer
    :   AT+ typeUnit
    ;

typeTuple
    :   CURLY_OPEN
        typeTupleElement?
        (COMMA typeTupleElement)*
        COMMA?
        CURLY_CLOSE
    ;

typeTupleElement
    :   variableAttribute* type
    ;

typeArray
    :   typeArrayPrefix*
        BRACKET_OPEN
        typeSizeComma?
        type
        BRACKET_CLOSE
    ;

typeArrayPrefix
    :   BRACKET_OPEN
        typeSize?
        BRACKET_CLOSE
    ;

typeDictionary
    :   typeDictionaryPrefix*
        BRACKET_OPEN
        typeSizeComma?
        type COMMA type
        BRACKET_CLOSE
    ;

typeDictionaryPrefix
    :   BRACKET_OPEN
        typeSizeComma?
        type
        BRACKET_CLOSE
    ;

typeSize
    :   L_INTEGER
    |   expressionRange
    ;

typeSizeComma
    :   typeSize
        COMMA
    ;

typeCallable
    :   PARENTHESES_OPEN
        genericParameters?
        parametersInterface?
        typeGuard?
        PARENTHESES_CLOSE
    ;

typePrimitive
    :   INTEGER
    |   FLOAT
    |   BOOL
    |   STRING
    |   CHAR
    ;

/* External unit types */

typeExternal
    :   typeCustom
    |   typeMethod
    |   typeProperty
    ;

typeCustom
    :   name
        genericArguments?
    ;

typeMethod
    :   typeCustom
        COLON
        IDENTIFIER
        PARENTHESES_OPEN
        PARENTHESES_CLOSE
    ;

typeProperty
    :   typeCustom
        COLON
        IDENTIFIER
    ;

/*******************************************************************************


    Parameters, arguments, blocks, generics and guards


*******************************************************************************/

/* Concrete parameters */

parameters
    :   PARENTHESES_OPEN
        parameterList?
        PARENTHESES_CLOSE
    ;

parameterList
    :   parameter
        (COMMA parameter)*
        COMMA?
    ;

parameter
    :   parameterSignature
        parameterDefault?
    ;

parameterSignature
    :   parameterAttribute*
        SPLAT?
        IDENTIFIER
        typeGuard?
    ;

parameterAttribute
    :   FINAL
    |   DYNAMIC
    ;

parameterDefault
    :   ASSIGN expression
    ;

/* Interface parameters */

parametersInterface
    :   parameterInterface
        (COMMA parameterInterface)*
        COMMA?
    ;

parameterInterface
    :   parameterAttribute*
        IDENTIFIER?
        typeGuard
    ;

/* Value arguements */

arguments
    :   argumentsNormal
    |   expressionLambdaConcise
    ;

argumentsNormal
    :   PARENTHESES_OPEN
        argumentList?
        typeGuard?
        PARENTHESES_CLOSE
    ;

argumentList
    :   argument
        (COMMA argument)*
        COMMA?
    ;

argument
    :   argumentName? expression
    ;

argumentName
    :   IDENTIFIER COLON
    ;

/* Code blocks */

codeBlock
    :   CURLY_OPEN
        statement*
        CURLY_CLOSE
    ;

codeConditional
    :   CURLY_OPEN
        declarationPrecondition*
        statement*
        declarationPostcondition*
        CURLY_CLOSE
    ;

codeBlockOrStatement
    :   codeBlock
    |   codeStatement
    ;

codeStatement
    :   ARROW statement
    ;

/* Generic parameters and arguments */

genericArguments
    :   BRACKET_OPEN
        genericArgument?
        (COMMA genericArgument)*
        COMMA?
        BRACKET_CLOSE
    ;

genericArgument
    :   genericAny
    |   genericInvariant
    |   genericCovariant
    |   genericContravariant
    ;

genericAny
    :   QUESTION
    ;

genericInvariant
    :   type
    ;

genericCovariant
    :   QUESTION
        COLON
        type
    ;

genericContravariant
    :   type
        COLON
        QUESTION
    ;

genericParameters
    :   BRACKET_OPEN
        genericParameter?
        (COMMA genericParameter)*
        COMMA?
        BRACKET_OPEN
    ;

genericParameter
    :   IDENTIFIER typeGuards?
    ;

/*******************************************************************************


    Statements


*******************************************************************************/

statement
    :   statementIf
    |   statementFor
    |   statementEach
    |   statementWhile
    |   statementMatch
    |   statementRescue
    |   statementRepeat
    |   statementEnsure
    |   statementScope
    |   statementTerminated
    |   statementExpression
    ;

/* If statement */

statementIf
    :   IF
        expression
        codeBlockOrStatement
        statementElseIf*
        statementElse?
    ;

statementElseIf
    :   ELSE IF
        expression
        codeBlockOrStatement
    ;

statementElse
    :   ELSE
        codeBlockOrStatement
    ;

/* For statement */

statementFor
    :   statementForSignature
        codeBlockOrStatement
    ;

statementForSignature
    :   FOR
        statementForControl
    ;

statementForControl
    :   expression? SEMICOLON
        expression? SEMICOLON
        expression? SEMICOLON?
    ;

/* Each statement */

statementEach
    :   statementEachSignature
        codeBlockOrStatement
    ;

statementEachSignature
    :   EACH statementEachParameters
        IN   expression (COMMA expression)* COMMA?
    ;

statementEachParameters
    :   statementEachParameter
        (COMMA statementEachParameter)*
        COMMA?
    ;

statementEachParameter
    :   variableAttribute*
        IDENTIFIER
        typeGuard?
    ;

/* While statement */

statementWhile
    :   statementWhileSignature
        codeBlockOrStatement
    ;

statementWhileSignature
    :   WHILE
        expression
    ;

/* Match statements */

statementMatch
    :   MATCH expression?
        statementMatchContents
    ;

statementMatchContents
    :   CURLY_OPEN
        statementMatchCase*
        statementMatchDefault?
        CURLY_CLOSE
    ;

statementMatchCase
    :   statementMatchCaseStatements
    |   statementMatchCaseStatement
    ;

statementMatchCaseStatements
    :   CASE expression COLON
        statement*
    ;

statementMatchCaseStatement
    :   CASE expression ARROW
        statement
    ;

statementMatchDefault
    :   statementMatchDefaultStatements
    |   statementMatchDefaultExpression
    ;

statementMatchDefaultStatements
    :   CASE ELSE COLON
        statement*
    ;

statementMatchDefaultExpression
    :   CASE ELSE ARROW
        statement
    ;

/* Exception handling statements */

statementRescue
    :   expression?
        RESCUE
        parameter?
        codeBlockOrStatement
    ;

statementRepeat
    :   expression?
        REPEAT
        parameterList
        codeBlockOrStatement
    ;

statementEnsure
    :   ENSURE
        codeBlockOrStatement
    ;

/* Opens a new exception block */

statementScope
    :   CURLY_OPEN
        statement*
        CURLY_CLOSE
    ;

/* Simple statements */

statementTerminated
    :   statementOne
        SEMICOLON
    ;

statementOne
    :   statementReturn
    |   statementYield
    |   statementThrow
    |   statementBreak
    |   statementContinue
    |   statementAssert
    |   statementWhen
    |   statementThen
    |   statementPass
    ;

statementReturn
    :   RETURN expression?
    ;

statementYield
    :   YIELD expression?
    ;

statementThrow
    :   THROW expression?
    ;

statementBreak
    :   BREAK expression?
    ;

statementContinue
    :   CONTINUE expression?
    ;

statementAssert
    :   ASSERT expression?
    ;

statementWhen
    :   expression WHEN expression
    ;

statementThen
    :   expression THEN expression
    ;

statementPass
    :   PASS
    ;

statementExpression
    :   expression SEMICOLON
    ;

/*******************************************************************************


    Expressions


*******************************************************************************/

expression
    :   expressionDeclaration
    ;

/* Variable declaration expression */

expressionDeclaration
    :   expressionAssignment
    |   variableAttribute* IDENTIFIER COLON type? ASSIGN expressionAssignment
    ;

variableAttribute
    :   FINAL
    |   DYNAMIC
    ;

/* Variable assignment expression */

expressionAssignment
    :   expressionTernary
    |   expressionAssignmentLocation ASSIGN expressionTernary
    ;

expressionAssignmentLocation
    :   IDENTIFIER
    |   expressionPrimary expressionAccessArray
    |   expressionPrimary expressionAccessProperty
    ;

/* Ternary expression */

expressionTernary
    :   expressionCoalescing
    |   expressionTernary QUESTION expression? COLON expressionCoalescing
    ;

/* Coalescing expression */

expressionCoalescing
    :   expressionLogicalOR
    |   expressionCoalescing COALESCING expressionLogicalOR
    ;

/* Logical expressions */

expressionLogicalOR
    :   expressionLogicalAND
    |   expressionLogicalOR LOGICAL_OR expressionLogicalAND
    ;

expressionLogicalAND
    :   expressionBitwiseOR
    |   expressionLogicalAND LOGICAL_AND expressionBitwiseOR
    ;

/* Bitwise expressions */

expressionBitwiseOR
    :   expressionBitwiseXOR
    |   expressionBitwiseOR BITWISE_OR expressionBitwiseXOR
    ;

expressionBitwiseXOR
    :   expressionBitwiseAND
    |   expressionBitwiseXOR BITWISE_XOR expressionBitwiseAND
    ;

expressionBitwiseAND
    :   expressionEquality
    |   expressionBitwiseAND BITWISE_AND expressionEquality
    ;

/* Comparison expressions */

expressionEquality
    :   expressionRelational
    |   expressionEquality EQUALS expressionRelational
    |   expressionEquality IDENTITY expressionRelational
    ;

expressionRelational
    :   expressionShift
    |   expressionRelational LT expressionShift
    |   expressionRelational LTOE expressionShift
    |   expressionRelational GT expressionShift
    |   expressionRelational GTOE expressionShift
    |   expressionRelational SPACESHIP expressionShift
    ;

/* Bitshift expressions */

expressionShift
    :   expressionAdditive
    |   expressionShift BITWISE_SHIFT_LEFT expressionAdditive
    |   expressionShift BITWISE_SHIFT_RIGHT expressionAdditive
    |   expressionShift BITWISE_SHIFT_APPEND expressionAdditive
    ;

/* Arithmetic expressions */

expressionAdditive
    :   expressionMultiplicative
    |   expressionAdditive ADD expressionMultiplicative
    |   expressionAdditive SUB expressionMultiplicative
    ;

expressionMultiplicative
    :   expressionUnary
    |   expressionMultiplicative MUL expressionUnary
    |   expressionMultiplicative EXP expressionUnary
    |   expressionMultiplicative MOD expressionUnary
    |   expressionMultiplicative DIV expressionUnary
    |   expressionMultiplicative INSTANCEOF expressionUnary
    ;

expressionUnary
    :   expressionRange
    |   LOGICAL_NOT expressionPrimary
    |   INC expressionPrimary
    |   DEC expressionPrimary
    |   ADD expressionPrimary
    |   SUB expressionPrimary
    |   BITWISE_NOT expressionPrimary
    |   AT expressionPrimary
    |   expressionPrimary INC
    |   expressionPrimary DEC
    |   expressionAccessMethod
    |   expressionAccessProperty
    |   expressionAccessMethod
    |   EQUALS expressionPrimary
    |   IDENTITY expressionPrimary
    |   LT expressionPrimary
    |   LTOE expressionPrimary
    |   GT expressionPrimary
    |   GTOE expressionPrimary
    |   SPACESHIP expressionPrimary
    |   BITWISE_SHIFT_LEFT expressionPrimary
    |   BITWISE_SHIFT_RIGHT expressionPrimary
    |   BITWISE_SHIFT_APPEND expressionPrimary
    |   INC
    |   DEC
    ;

/* Range expression */

expressionRange
    :   expressionPrimary
    |   expressionPrimary RANGE expressionPrimary
    |   expressionPrimary PERIOD expressionPrimary PERIOD expressionPrimary
    ;

/* Atomic expressions */

expressionPrimary
    :   SELF
    |   SUPER
    |   RETURN
    |   name
    |   expressionObject
    |   expressionArray
    |   expressionTable
    |   expressionTuple
    |   expressionLambda
    |   expressionGenerator
    |   expressionMatch
    |   expressionFunction
    |   expressionLiteral
    |   expressionParenthesized
    |   expressionPrimary expressionAccess
    ;

/* Object expressions */

expressionObject
    :   NEW
        expressionObjectName
        genericArguments?
        arguments?
        (
            CURLY_OPEN
            declarationClassMember
            CURLY_CLOSE
        )?
    ;

expressionObjectName
    :   name
    |   TYPE
    |   SUPER
    |   SELF
    ;

/* Array expression */

expressionArray
    :   expressionArrayLiteral
    |   expressionArrayConstruction
    ;

expressionArrayLiteral
    :   BRACKET_OPEN
        expression? (COMMA expression)* COMMA?
        BRACKET_CLOSE
    ;

expressionArrayConstruction
    :   NEW
        typeArray
        arguments?
    ;

/* Table expression */

expressionTable
    :   expressionTableLiteral
    |   expressionMapConstruction
    ;

expressionTableLiteral
    :   BRACKET_OPEN
        expressionTableLiteralPairs?
        BRACKET_CLOSE
    ;

expressionTableLiteralPairs
    :   expressionTableLiteralPair
        (COMMA expressionTableLiteralPair)*
        COMMA?
    ;

expressionTableLiteralPair
    :   expression
        COLON
        expression
    ;

expressionMapConstruction
    :   NEW
        typeDictionary
        arguments?
    ;

/* Tuple expression */

expressionTuple
    :   CURLY_OPEN
        argumentList
        CURLY_CLOSE
    ;

/* Access expression */

expressionAccess
    :   expressionAccessMethod
    |   expressionAccessProperty
    |   expressionAccessArray
    |   expressionAccessConditional
    ;

expressionAccessMethod
    :   PERIOD IDENTIFIER
        genericArguments?
        arguments
    ;

expressionAccessProperty
    :   PERIOD IDENTIFIER (PERIOD GET)?
    ;

expressionAccessArray
    :   BRACKET_OPEN
        expression
        BRACKET_CLOSE
    ;

expressionAccessConditional
    :   QUESTION
        expressionAccessCondition?
    ;

expressionAccessCondition
    :   PARENTHESES_OPEN
        expression
        expressionAccessConditionReturn?
        PARENTHESES_CLOSE
    ;

expressionAccessConditionReturn
    :   COLON expression
    ;

/* Lambda expression */

expressionLambda
    :   expressionLambdaExpansive
    |   expressionLambdaConcise
    ;

expressionLambdaExpansive
    :   genericParameters?
        parameters
        typeGuard?
        codeBlockOrStatement
    ;

expressionLambdaConcise
    :   PARENTHESES_OPEN
        ARROW
        expressionLambdaConciseContents
        PARENTHESES_CLOSE
    ;

expressionLambdaConciseContents
    :   expression
    |   (statement* statementOne?)
    ;

/* Generator expression */

expressionGenerator
    :   expressionGeneratorFor
    |   expressionGeneratorEach
    |   expressionGeneratorWhile
    ;

expressionGeneratorFor
    :   PARENTHESES_OPEN
        expression
        statementForSignature
        PARENTHESES_CLOSE
    ;

expressionGeneratorEach
    :   PARENTHESES_OPEN
        expression
        statementEachSignature
        PARENTHESES_CLOSE
    ;

expressionGeneratorWhile
    :   PARENTHESES_OPEN
        expression
        statementWhileSignature
        PARENTHESES_CLOSE
    ;

/* Match expression */

expressionMatch
    :   statementMatch
    ;

/* Function expression */

expressionFunction
    :   name
        genericArguments?
        arguments
    ;

/* Literal expressions */

expressionLiteral
    :   L_STRING
    |   L_CHAR
    |   L_BOOL
    |   L_FLOAT
    |   L_INTEGER
    |   L_NULL
    ;

/* Parameterized expressions */

expressionParenthesized
    :   PARENTHESES_OPEN
        expression
        PARENTHESES_CLOSE
    ;

/*******************************************************************************


    Operators.


*******************************************************************************/

ASSIGN
    :   '='
    ;

ASSIGNS
    :   '+='
    |   '-='
    |   '*='
    |   '/='
    |   '%='
    |   '<=>='
    |   '<<='
    |   '>>='
    |   '>>>='
    |   '&='
    |   '|='
    ;

INSTANCEOF
    :   'instanceof'
    |   '!instanceof'
    ;

COALESCING
    :   '??'
    ;

/* Arithmetic operators */

ADD
    :   '+'
    ;

SUB
    :   '-'
    ;

MUL
    :   '*'
    ;

EXP
    :   '**'
    ;

DIV
    :   '/'
    ;

MOD
    :   '%'
    ;

INC
    :   '--'
    ;

DEC
    :   '++'
    ;

/* Comparison operators */

LT
    :   '<'
    ;

GT
    :   '>'
    ;

LTOE
    :   '<='
    ;

GTOE
    :   '>='
    ;

SPACESHIP
    :   '<=>'
    ;

EQUALS
    :   '=='
    |   '!='
    ;

IDENTITY
    :   '==='
    |   '!=='
    ;

/* Logical operators */

LOGICAL_NOT
    :   '!'
    ;

LOGICAL_AND
    :   '&&'
    ;

LOGICAL_OR
    :   '||'
    ;

/* Bitwise operators */

BITWISE_NOT
    :   '~'
    ;

BITWISE_AND
    :   '&'
    ;

BITWISE_OR
    :   '|'
    ;

BITWISE_XOR
    :   '^'
    ;

BITWISE_SHIFT_LEFT
    :   '<<'
    ;

BITWISE_SHIFT_RIGHT
    :   '>>'
    ;

BITWISE_SHIFT_APPEND
    :   '>>>'
    ;

/*******************************************************************************


    Reserved keywords.


*******************************************************************************/

AS
    :   'as'
    ;

ASSERT
    :   'assert'
    ;

BREAK
    :   'break'
    ;

CASE
    :   'case'
    ;

CLASS
    :   'class'
    ;

CONSTANT
    :   'constant'
    ;

CONSTRUCTOR
    :   'constructor'
    ;

CONTINUE
    :   'continue'
    ;

DELEGATE
    :   'delegate'
    ;

DYNAMIC
    :   'dynamic'
    ;

EACH
    :   'each'
    ;

ELSE
    :   'else'
    ;

ENSURE
    :   'ensure'
    ;

ENUMERATION
    :   'enumeration'
    ;

EXTEND
    :   'extend'
    ;

FINAL
    :   'final'
    ;

FOR
    :   'for'
    ;

FUNCTION
    :   'function'
    ;

GET
    :   'get'
    ;

IF
    :   'if'
    ;

IMMUTABLE
    :   'immutable'
    ;

IN
    :   'in'
    ;

INLINE
    :   'inline'
    ;

INTERFACE
    :   'interface'
    ;

INVARIANT
    :   'invariant'
    ;

LAZY
    :   'lazy'
    ;

MATCH
    :   'match'
    ;

MODULE
    :   'module'
    ;

MUTABLE
    :   'mutable'
    ;

NEW
    :   'new'
    ;

PASS
    :   'pass'
    ;

PRIVATE
    :   'private'
    ;

PROTECTED
    :   'protected'
    ;

PUBLIC
    :   'public'
    ;

PURE
    :   'pure'
    ;

RESCUE
    :   'rescue'
    ;

REPEAT
    :   'repeat'
    ;

REQUIRE
    :   'require'
    ;

RETURN
    :   'return'
    ;

SELF
    :   'self'
    ;

SET
    :   'set'
    ;

STRUCTURE
    :   'structure'
    ;

SUPER
    :   'super'
    ;

TEST
    :   'test'
    ;

THEN
    :   'then'
    ;

THROW
    :   'throw'
    ;

TYPE
    :   'type'
    ;

TRAIT
    :   'trait'
    ;

UNSAFE
    :   'unsafe'
    ;

USE
    :   'use'
    ;

VARIABLE
    :   'variable'
    ;

WHEN
    :   'when'
    ;

WHILE
    :   'while'
    ;

YIELD
    :   'yield'
    ;

/* Primitive date types */

FLOAT
    :   'float'
    |   'f32'
    |   'f64'
    ;

STRING
    :   'string'
    ;

INTEGER
    :   'int'
    |   'i8'
    |   'i16'
    |   'i32'
    |   'i64'
    |   'uint'
    |   'u8'
    |   'u16'
    |   'u32'
    |   'u64'
    ;

BOOL
    :   'bool'
    ;

CHAR
    :   'char'
    ;

/*******************************************************************************


    Symbols


*******************************************************************************/

AT
    :   '@'
    ;

HASH
    :   '#'
    ;

ARROW
    :   '->'
    ;

COLON
    :   ':'
    ;

DOUBLECOLON
    :   '::'
    ;

SEMICOLON
    :   ';'
    ;

QUOTE_SINGLE
    :   '\''
    ;

QUOTE_DOUBLE
    :   '"'
    ;

PARENTHESES_OPEN
    :   '('
    ;

PARENTHESES_CLOSE
    :   ')'
    ;

BRACKET_OPEN
    :   '['
    ;

BRACKET_CLOSE
    :   ']'
    ;

CURLY_OPEN
    :   '{'
    ;

CURLY_CLOSE
    :   '}'
    ;

PERIOD
    :   '.'
    ;

RANGE
    :   '..'
    ;

SPLAT
    :   '...'
    ;

COMMA
    :   ','
    ;

QUESTION
    :   '?'
    ;

BACKSLASH
    :   '\\'
    ;

/*******************************************************************************


    Literals


*******************************************************************************/

L_NULL
    :   'null'
    ;

L_BOOL
    :   'true'
    |   'false'
    ;

/* String literals */

L_STRING
    :   QUOTE_DOUBLE
        (ESCAPED_CHAR | ~["])*
        QUOTE_DOUBLE
    ;

/* Char literals */

L_CHAR
    :   QUOTE_SINGLE
        (ESCAPED_CHAR | ~['\\])
        QUOTE_SINGLE
    ;

/* Integer literals */

L_INTEGER
    :   NUMERAL_PREFIX?
        L_INTERGER_BASE?
        L_INTEGER_NUMERAL
        L_INTEGER_CAST?
    ;

L_INTERGER_BASE
    :   [0-9]+ HASH
    ;

L_INTEGER_NUMERAL
    :   [0-9][0-9a-zA-Z_]*
    ;

L_INTEGER_CAST
    :   HASH INTEGER
    ;

/* Float literals */

L_FLOAT
    :   L_FLOAT_NUMERAL
        L_FLOAT_EXPONENT?
        L_FLOAT_CAST?
    ;

L_FLOAT_NUMERAL
    :   [0*9_]+
        PERIOD
        [0*9_]+
    ;

L_FLOAT_EXPONENT
    :   [eE]
        NUMERAL_PREFIX?
        [0*9_]+
    ;

L_FLOAT_CAST
    :   HASH FLOAT
    ;

fragment
NUMERAL_PREFIX
    :   '+'
    |   '-'
    ;

ESCAPED_CHAR
    :   BACKSLASH .
    ;

IDENTIFIER
    :   [a-zA-Z_] [a-zA-Z0-9_]*
    ;

WHITESPACE
    :   [ \t\r\n]+ -> skip
    ;

COMMENT_BLOCK
    :   '/*' (COMMENT_BLOCK|.)*? '*/' -> skip
    ;

COMMENT_LINE
    :   '//' .*? (NEWLINE|EOF) -> skip
    ;

NEWLINE
    :   '\r' ? '\n'
    ;

