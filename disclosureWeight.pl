%
/*{==============================================================
 |
 |  attributeWeight(Type, Technology, Issuer, AttributeName, Weight)
 |    inputs: Type, Technology and Issuer of the credential
 |    outputs: weight of the specified credential
 |    effects: allows to define and find a weight linked to a specific attribute in the context of the credential it is recorded in
 |
 +================================================================}*/
 
:- dynamic(attributeWeight/5).


/*{--------------------------------------------------------------
 | currentWeightCount will contain the current value of the weights sum
+----------------------------------------------------------------}*/
:- dynamic(currentWeightCount/1).
currentWeightCount(0).

:- dynamic(elementCounter/1).
elementCounter(1).





/*{===============================================================
 |
 |  showCredCount(User, SP, TransactionID, Cred, ListOfAttrsToBeDisclosed)
 |    inputs: User, SP, TransactionID, Cred, ListOfAttrsToBeDisclosed 
 |    outputs: ---
 |
 +=============================================================}*/
	
	
showCredCount(User, SP, TransactionID, Cred, List) :-
	write('***************************************************'), nl,
	write('first rule of show cred (X.509): '), write(Cred), nl,
	write('***************************************************'), nl,
	Cred,
	Cred = cred(_, 'X.509', _, ListOfCredAttrs), !,
	usersCred(User, Cred),
	write('***************************************************'), nl,
	write('DEBUG: Will be showing: '), write(Cred), nl,
	write('***************************************************'), nl,
	createListOfAttrNames(ListOfCredAttrs, NewList),
	showCredAttrsCount(User, SP, TransactionID, Cred, NewList)
	.

showCredCount(User, SP, TransactionID, Cred, 'Ownership') :-
	write('***************************************************'), nl,
	write('second rule of show cred (ownership): '), write(Cred), nl,
	write('***************************************************'), nl,
	!, 
	(
		(
		usersCred(User, Cred), 
		getCredAttribute(Cred, 'Issuer', Issuer),
		addToCounter(SP, TransactionID, attr('Ownership', 'Verified'), credSource(Technology, Type, Issuer))
		)
	;
		(
		write('ERROR: User does not posses the required credential.')
		)
	)
	.
	
	
showCredCount(User, SP, TransactionID, Cred, List) :-
	write('***************************************************'), nl,
	write('DEBUG: The credential to be shown is IDEMIX '), nl,
	write('***************************************************'), nl,
	Cred,
	usersCred(User, Cred),
	showCredAttrsCount(User, SP, TransactionID, Cred, List)
	.



	

%----------------------------------------------------------------



showCredAttrsCount(_, _, _, _, []) :- 
	write('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'), nl, 
	write('The disclosure cost is: '), nl,
	currentWeightCount(Cost),
	write(Cost), nl,
	write('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'), nl
	.




showCredAttrsCount(User, SP, TransactionID, Cred, [First|Rest]) :-
	ground(TransactionID), !, 
	!,
	(
		transactionID(TransactionID), write('DEBUG: TransactionID exists. '), nl, !
	;
		assert(transactionID(TransactionID)), write('DEBUG: Asserted the new transactionID. '), nl
	),
	getCredAttribute(Cred, First, AttrValue),
	cred(_, Technology, Type, _),
	Cred = cred(_, Technology, Type, _),
	usersCred(User, Cred),
	getCredAttribute(Cred, 'Issuer', Issuer),
	addToCounter(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	showCredAttrsCount(User, SP, TransactionID, Cred, Rest)
	.
	
/*-------in case TID needs to be created--------*/
showCredAttrsCount(User, SP, TransactionID, Cred, [First|Rest]) :-
	startTransaction(User, SP, TransactionID),
	assert(transactionID(TransactionID)),
	getCredAttribute(Cred, First, AttrValue),
	cred(_, Technology, Type, _),
	Cred = cred(_, Technology, Type, _),
	usersCred(User, Cred),
	getCredAttribute(Cred, 'Issuer', Issuer),
	addToCounter(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	showCredAttrsCount(User, SP, TransactionID, Cred, Rest).
	

	

addToCounter(_, _, [], _):- !.



addToCounter(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)):-
			write(addToCounter(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer))), nl, 
			currentWeightCount(Cost),
			attributeWeight(Type, Technology, Issuer, First, Weight),
			NewCost is Cost + Weight,
			retract(currentWeightCount(_)),
			assert(currentWeightCount(NewCost))
			.


