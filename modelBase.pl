:- dynamic(cred/4).
:- dynamic(definedCredType/3).
:- dynamic(credSetAttribute/5).
:- dynamic(prefixOfAttrName/4).
:- dynamic(listOfExistingAliases/1).
:- dynamic(definedCredTemplate/3).
:- dynamic(usersCred/2).



/*{==========================================================
 |
 | Structure attrDef:
 | attrDef(AttrName, AttrType, UserKnowledge, IssuerKnowledge, Uniqueness).
 |
 %============================================================}*/
%
/*{==========================================================
 | 
 | Structure attr:
 | attr(attrName, attrValue(ValueType, Value)).
 | 
%============================================================}*/
%


/*{==========================================================
 |
 | defineCredType(Technology, Type, AttributeDefinitions)
 |
 |	input:  Technology, Type, AttributeDefinitions
 |	output: ---
 |	effect: asserts 'definedCredType' with the given parameters from the input
 |
 |  ---> checks if there is an existing credential definition for the given technology-type combination
 |	---> creates a credential template with the given attributes and default ones according to the technology
 |	---> creates getters and setters for the credential attributes
 |
%==========================================================}*/


defineCredType(Technology, Type, AttributeDefinitions) :-
	nl,
	write('Defining the specified credential type:'), nl,
	write(defineCredType(Technology, Type)), nl, 
		((
			definedCredType(Technology, Type, _), 
			write('ERROR: Type already defined.'), !
		)
		;
		((
				Technology = 'Idemix', !, 
				(
					assert(definedCredType(Technology, Type, [attrDef('IssuerSig', 'Signature', 'Known', 'SPChosen', 'Unique'),
											attrDef('Issuer', 'ID', 'Known', 'SPChosen', 'Shared'),
											attrDef('ExpiryDate', 'Date', 'Known', 'SPChosen', 'Shared'),
											attrDef('UserSecret', 'SecretKey', 'UserChosen', 'Hidden', 'Shared')
											| AttributeDefinitions])),
					assert(prefixOfAttrName(Technology, Type, 'IssuerSig', 0)),
					assert(prefixOfAttrName(Technology, Type, 'Issuer', 1)),
					assert(prefixOfAttrName(Technology, Type, 'ExpiryDate', 2)),
					assert(prefixOfAttrName(Technology, Type, 'UserSecret', 3)),
					assignPrefixes(Technology, Type, AttributeDefinitions, 4),
					
					attrDefToAttr(AttributeList, AttributeDefinitions), 
					assert(definedCredTemplate(Technology, Type, 
						[attr('IssuerSig', _), attr('Issuer', _), attr('ExpiryDate', _), attr('UserSecret', _) | AttributeList]))
				)
			;
				Technology = 'UProve', !, 
				(
					assert(definedCredType(Technology, Type, [attrDef('IssuerSig', 'Signature', 'Known', 'SPChosen', 'Unique'),
											attrDef('Issuer', 'ID', 'Known', 'SPChosen', 'Shared'),
											attrDef('ExpiryDate', 'Date', 'Known', 'SPChosen', 'Shared'),
											attrDef('UserSecret', 'SecretKey', 'UserChosen', 'Hidden', 'Shared') |  AttributeDefinitions])),
					assert(prefixOfAttrName(Technology, Type, 'IssuerSig', 0)),
					assert(prefixOfAttrName(Technology, Type, 'Issuer', 1)),
					assert(prefixOfAttrName(Technology, Type, 'ExpiryDate', 2)),
					assert(prefixOfAttrName(Technology, Type, 'UserSecret', 3)),
					assignPrefixes(Technology, Type, AttributeDefinitions, 4),
					
					attrDefToAttr(AttributeList, AttributeDefinitions), 
					assert(definedCredTemplate(Technology, Type, 
						[attr('IssuerSig', _), attr('Issuer', _), attr('ExpiryDate', _), attr('UserSecret', _) | AttributeList]))
				)
			;
				Technology = 'X.509', !, 
				(
					assert(definedCredType(Technology, Type, [attrDef('IssuerSig', 'Signature', 'Known', 'SPChosen', 'Unique'),
											attrDef('Issuer', 'ID', 'Known', 'SPChosen', 'Shared'),
											attrDef('ExpiryDate', 'Date', 'Known', 'SPChosen', 'Shared') |  AttributeDefinitions])),
					assert(prefixOfAttrName(Technology, Type, 'IssuerSig', 0)),
					assert(prefixOfAttrName(Technology, Type, 'Issuer', 1)),
					assert(prefixOfAttrName(Technology, Type, 'ExpiryDate', 2)),
					assignPrefixes(Technology, Type, AttributeDefinitions, 3),

					write('DEBUG: Creating the list of attr structures. '), nl,
					attrDefToAttr(AttributeList, AttributeDefinitions), 
					write('DEBUG: Successful creation'), nl,
					assert(definedCredTemplate(Technology, Type, 
						[attr('IssuerSig', _), attr('Issuer', _), attr('ExpiryDate', _) | AttributeList]))
				)
			;
				write('ERROR: unspecified technology.'), nl
		),
		write('Completed.'), nl
		)
		)
	.
	
	
/*{==========================================================
 |
 | attrDefToAttr(AttributeList, AttributeDefinitionsList)
 |
 |	input:  AttributeList, AttributeDefinitionsList
 |	output: 
 |	effect: sets the AttributeList according to the list of attribute definitions
 | 
 |  -->used for initialising a credential template based on a defined credential type
 |
%==========================================================}*/

attrDefToAttr([], []) :- !.

attrDefToAttr([attr(AttrName, _)], [attrDef(AttrName, _, _, _, _)]) :- !.

attrDefToAttr([attr(AttributeName, _) | Rest], [attrDef(AttributeName, _, _, _, _) | RestDef]) :- 
	attrDefToAttr(Rest, RestDef).
	

/*{==========================================================
 |
 | issueCred(User, Alias, Technology, Type, Issuer, ExpiryDate, Attributes, Cred)
 |
 |	input:  User, Alias, Technology, Type, Issuer, ExpiryDate, Attributes (in the form of [attr('Name', Value), ..])
 |	output: initialised credential Cred
 |	effect: creates a new signed Credential of the correct type and with the given attributes
 |
 |  --> the specified credential is asserted in the database
 |
%==========================================================}*/

issueCred(User, Alias, Technology, Type, Issuer, ExpiryDate, Attributes, Cred) :-
	write('Issuing the credential:'), nl, 
	write(issueCred(User, Alias, Technology, Type, Issuer, ExpiryDate, Attributes, Cred)), nl, 
	definedCredTemplate(Technology, Type, AttrList),
	write('DEBUG: Found the defined credential template'), nl,
	(
	definedCredType(Technology, Type, _), !, 
	%write('DEBUG: found a defined credential type'), nl,
	%write(definedCredType(Technology, Type, _)), nl,
	Cred = cred(Alias, Technology, Type, AttrList),
		(
			(
			listOfExistingAliases(Alias) -> write('ERROR: Alias already exists.'), nl,
			nl			
			)
		;
			(
			createNewSignature(Issuer, Signature),
			%credSetAttributes(Cred, [attr('IssuerSig', value('Sig1', 'Signature')), attr('Issuer', value(Issuer, 'ID')), attr('ExpiryDate', value(ExpiryDate, 'Date')) | Attributes]), 
			credSetAttributes(Cred, [attr('IssuerSig', Signature), attr('Issuer', Issuer), attr('ExpiryDate', ExpiryDate) | Attributes]), 
			assert(listOfExistingAliases(Alias)),
			write('The issued credential is: '), nl,
			write(Cred), nl,
			assert(Cred)
			)
		)
	),
	write('DEBUG:Recording the user ownership. '), nl,
	assert(usersCred(User, Cred)),
	write('DEBUG: User ownership recorded. '), nl, nl,
	write('----------------------------------------------------------------------'), nl, nl
	.


/*{+==============================================================
 |
 |  createNewSignature (Issuer, Signature)
 |    inputs: Issuer
 |    outputs: Signature
 |    effects: generates a fresh unique signature
 |
 |  --> signature is derived from Issuer and unique number
 |
 +==============================================================}*/

createNewSignature(Issuer, Signature) :-
	getSigCount(SigCounter),
	atomic_list_concat(['Sig_', Issuer, '_', SigCounter], Signature).
	
	
/*{--------------------------------------------------------------
 | SigCount counts the signatures 
+----------------------------------------------------------------}*/
:-dynamic(sigCount/1).
sigCount(1).

	
/*{===============================================================
 |
 |  getSigCount(Count)
 |    inputs: ---
 |    outputs: Count
 |    effects: returns the current signature count and updates the count
 |
 +=============================================================}*/

getSigCount(Count) :-
	sigCount(Count),
	Count1 is Count + 1,
   retract(sigCount(_)),
	assert(sigCount(Count1)).


/*{+==============================================================
 |
 |  credSetAttributes(Credential, ValueList)
 |    inputs: Credential, ValueList
 |    outputs: ---
 |    effects: sets the attribute values(ValueList) in the Credential
 |
 |  --> calls credSetAttribute for every value in the list
 |  --> the Technology and Type of the credential can be extracted from the given credential, and are therefore not a separate input
 |
 +==============================================================}*/

credSetAttributes(_, []) :- write('trying the first branch of credsetattribute'), nl, nl, !.
credSetAttributes(Cred, [attr(AttrName, AttrValue) | Rest]) :-
	nl, nl, 
	%write('DEBUG: Entered the credSetAttributes'), nl, nl, 
	Cred = cred(_, Technology, Type, _),
	credSetAttribute(Technology, Type, Cred, AttrName, AttrValue),
	%write('DEBUG: Setting the rest of the attributes'), nl,
	credSetAttributes(Cred, Rest).

	
	
/*{+==============================================================
 |
 |  credSetAttribute(Technology, Type, Credential, AttributeName, AttributeValue)
 |    inputs: Technology, Type, Credential, AttributeName, AttributeValue
 |    outputs: ---
 |    effects: sets the given attribute value in the Credential
 |
 |  --> the Credential input contains the information on the credential technology and type
 |
 +==============================================================}*/

	
credSetAttribute(Technology, Type, cred(_, Technology, Type, [First|Rest]), AttributeName, AttributeValue) :-
	%write('setting the attribute: '), write(AttributeName), nl, nl, 
	%write(credSetAttribute(Technology, Type, cred(_, Technology, Type, [First|Rest]), AttributeName, AttributeValue)),nl,nl,
	prefixOfAttrName(Technology, Type, AttributeName, PrefixLength),
	(	
		(
		PrefixLength == 0, 
		%write('The attribute is now the first following element'), nl, 
		First = attr(AttributeName, AttributeValue), !
		)
	;
		(
		%write('Moving on in the list to set the element:'), 
		%write(AttributeName), nl, nl, 
		NewPrefixLength is PrefixLength - 1, 
		%write('DEBUG: New prefix is: '), write(NewPrefixLength), nl,
		%write('DEBUG: checking the input for assigning the values: '),
		%write(assignValueToElement(Rest, AttributeValue, NewPrefixLength)), nl,
		assignValueToElement(Rest, AttributeValue, NewPrefixLength),
		%write('DEBUG: Assigned value to element:'), nl,
		%write(assignValueToElement(Rest, AttributeValue, NewPrefixLength)), 
		nl
		)
	),
	write('Assigned attribute value to: '),
    write(AttributeName), nl, nl.


/*{+==============================================================
 |
 |   assignValueToElement (AttrList, AttrValue, Prefix)
 |     input: AttrList, AttrValue, Prefix
 |     output: ---
 |     effects: assigns the given value to the attribute in the AttrList at position defined by Prefix 
 |
 |   --> Prefix = number of predecessors of the target attribute of the list
 |
 +==============================================================}*/


assignValueToElement([attr(_, AttributeValue)|_], AttributeValue, 0) :-
    !.

	
		
assignValueToElement([_|Rest], AttributeValue, Prefix) :-
		NewPrefix is Prefix - 1,
        assignValueToElement(Rest, AttributeValue, NewPrefix).



/*{+==============================================================
 |
 |   assignPrefixes (Technology, Type, AttributeDefinitions, StartingPrefix)
 |     input: Technology, Type, AttributeDefinitions, StartingPrefix
 |     output: ---
 |     effects: asserts 'prefixOfAttrName'-clauses
 |
 |   --> StartingPrefix = number of predecessors of the current attribute in the credential
 |
 +==============================================================}*/

assignPrefixes(_, _, [], _) :- !.

assignPrefixes(Technology, Type, attrDef(AttrName, _, _, _, _), StartingPrefix) :- 
	assert(prefixOfAttrName(Technology, Type, AttrName, StartingPrefix)).

assignPrefixes(Technology, Type, [attrDef(AttrName, _, _, _, _)|Rest], StartingPrefix) :-
	assert(prefixOfAttrName(Technology, Type, AttrName, StartingPrefix)),
	NewPrefix is StartingPrefix + 1,
	assignPrefixes(Technology, Type, Rest, NewPrefix).




/*{+==============================================================
 |
 |   genCredSets (Technology, Type, AttributeList, PrefixLength)
 |     input: Technology, Type, AttibuteList, PrefixLength
 |     output: ---
 |     effects: asserts 'credSetAttribute'-clauses
 |
 |   --> generates the credSetAttribute clauses for a credential
 |   --> PrefixLength = number of predecessors of the current attribute in the credential
 |
 +==============================================================}*/

genCredSets(Technology, Type, [], _) :-
		!,
		assert((credSetAttribute(Type, _, AttrName, Attr) :-
                  error('Attribute ', [AttrName, ': not an attribute of the credential']))).
				  
genCredSets(Type, [attrDef(AttrName, AttrType, _, _, _) | Rest], PrefixLen) :-
	genPrefix(PrefixLen, Prefix, value(AttrValue1, AttrType1)),
	assert((credSetAttribute(Type, cred(Alias, Technology, Type, Prefix), AttrName, value(AttrValue2, AttrType2)) :-
                !,
                (
							ground(AttrValue1), !, error('Attribute has already a value assigned', [AttrName, ': ', AttrValue1, ' <--> ', AttrValue2])
                ;  	AttrType1 = AttrType2, !, AttrValue1 = AttrValue2
                ;  	error('Incompatible types in assignment', [AttrName, ': ', AttrType1, ' <--> ', AttrType2])
                ))),
	PrefixLen1 is PrefixLen + 1,
	genCredSets(Type, Rest, PrefixLen1).


/*{+==============================================================
 |
 |   setAttributesValues (AttributeList, PrefixLength, AttrValue)
 |     input: AttributeList, PrefixLength, AttrValue
 |     output: ---
 |     effects: asserts 'credSetAttribute'-clauses
 |
 |   --> generates the credSetAttribute clauses for a credential
 |   --> PrefixLength = number of predecessors of the current attribute in the credential
 |
 +==============================================================}*/

setAttributesValues([], _, _) :- !.

setAttributesValues([First|Rest], PrefixLength, AttrValue) :-
	%write('DEBUG: In the setAttributesValues'), nl, nl, 
	(
		PrefixLength == 0,
		setAttrValue(First, AttrValue)
	)
	;
	(
		NewPrefixLength is PrefixLength - 1,
		setAttributesValues(Rest, NewPrefixLength, AttrValue)
	).

setAttrValue(attr(_, AttrValue), AttrValue).

