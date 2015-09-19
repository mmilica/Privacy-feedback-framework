:-dynamic(transactionID/1).

:-dynamic(collaboratingSPs/2).

:- dynamic(linkingReason/1).


/*{==============================================================
 |
 | isLinked(SP, TransactionId1, TransactionId2, Reason)
 |    inputs: SP, TID1, TID2, Reason
 |    outputs: ---
 |    effects: represents a link between two transactionIds
 |
 +================================================================}*/
:-dynamic(isLinked/4).


	
/*{+==============================================================
 |
 | profile(SP, TransactionId, Attribute, Source)
 |    inputs: SP, linked Attribute and TransactionId and the source (representing trustworhiness)
 |    outputs: ---
 |    effects: Profile predicate represents a link between a transactionId and an attribute
 | 
 |	--> A profile is uniquely identified with a transactionID
 |
 +================================================================}*/
	
:-dynamic(dynamicProfile/4).

profile(SP, A, B, Source):-
	dynamicProfile(SP, A, B, Source).
%profile(SP, A, B, Source):-
%	staticProfile(SP, A, B, Source).

	
:- [transactionID].


/* membership test */
member(Atom, [Atom | _]).
member(Atom, [_ | Rest]) :- member(Atom, Rest).


/*------------------------------------------------------------------------------*/
%
/*uniqueSet takes as input a list of attributes which are considered to uniquely identify an individual*/
%
/*------------------------------------------------------------------------------*/	

:-dynamic(uniqueSet/1).

:-dynamic(uniqueAttr/1).

:-dynamic(isUnique/2).


	
/*{===============================================================
 |
 |  lookupCred(Alias, Type, Cred)
 |    inputs: Alias, Type 
 |    outputs: Cred
 |    effects: returns an existing credential with the specified alias and of the specified type
 |
 +=============================================================}*/

lookupCred(Alias, Type, Cred) :-
    cred(Alias, Technology, Type, Attrs),
    Cred = cred(Alias, Technology, Type, Attrs).
	
	

/*------------------------------------------------------------------------------*/
%
/*Searching for credential attributes*/
%
/*------------------------------------------------------------------------------*/	
%
	
/*{===============================================================
 |
 |  getCredAttribute(Cred, AttrName, AttrValue)
 |    inputs: Cred, AttrName 
 |    outputs: AttrValue
 |    effects: returns the value of the attribute specified with AttrName
 |
 +=============================================================}*/

getCredAttribute(cred(Alias, Technology, Type, AttrList), AttrName, AttrValue) :-
	%write('DEBUG: Getting cred attr: '),nl,
	%write(cred(Alias, Technology, Type, AttrList)), nl, 
	cred(Alias, Technology, Type, AttrList),
	prefixOfAttrName(Technology, Type, AttrName, Prefix),
	getAttrFromList(AttrList, Prefix, attr(AttrName, AttrValue))
	.
	
	
/*{===============================================================
 |
 |  getAttrFromList(AttrList, Prefix, Attr)
 |    inputs: AttrList, Prefix 
 |    outputs: Attr
 |    effects: returns the attribute with the specified prefix from the givel list of attributes
 |
 +=============================================================}*/
	
getAttrFromList([], _, _) :- 
	!, 
	write('ERROR: The search for a credential attribute has reached the end of attributes list, prefix is too long.')
	.
	
getAttrFromList([Attr|Rest], 0, Attr) :- !.
		
getAttrFromList([First|Rest], Prefix, Attr) :-
	NewPrefix is Prefix - 1,
	getAttrFromList(Rest, NewPrefix, Attr).

	

	
/*{===============================================================
 |
 |  showCred(User, SP, TransactionID, Cred, Attribute-Property)
 |    inputs: User, SP, TransactionID, Cred, Attribute-Property 
 |    outputs: ---
 |
 +=============================================================}*/
	
%

/*{===============================================================
 |
 |  showCred(User, SP, TransactionID, Cred, ListOfAttrsToBeDisclosed)
 |    inputs: User, SP, TransactionID, Cred, ListOfAttrsToBeDisclosed 
 |    outputs: ---
 |
 +=============================================================}*/
	
	
showCred(User, SP, TransactionID, Cred, List) :-
	write('***************************************************'), nl,
	write('first rule of showCred (X509): '), write(Cred), nl,
	write('***************************************************'), nl,
	Cred,
	Cred = cred(_, 'X509', _, ListOfCredAttrs), !,
	usersCred(User, Cred),
	write('***************************************************'), nl,
	write('DEBUG: Will be showing: '), write(Cred), nl,
	write('***************************************************'), nl,
	Cred,
	write('DEBUG: Showing the cred attributes: '), write(List), nl,
	write('DEBUG: Creating a list of attrs: '), write(createListOfAttrNames(ListOfCredAttrs, NewList)), nl, 
	createListOfAttrNames(ListOfCredAttrs, NewList),
	write('***************************************************'), nl,
	write('Jumping into:'), nl, 
	write(showCredAttrs(User, SP, TransactionID, Cred, NewList)), nl,
	write('***************************************************'), nl,
	showCredAttrs(User, SP, TransactionID, Cred, NewList)
	.

showCred(User, SP, TransactionID, Cred, 'Ownership') :-
	write('***************************************************'), nl,
	write('second rule of showCred (ownership): '), write(Cred), nl,
	write('***************************************************'), nl,
	!, 
	(
		(
		usersCred(User, Cred), 
		getCredAttribute(Cred, 'Issuer', Issuer),
		addToProvidersDB(SP, TransactionID, attr('Ownership', 'Verified'), credSource(Technology, Type, Issuer))
		)
	;
		(
		write('ERROR: User does not posses the required credential.')
		)
	).
	
	
showCred(User, SP, TransactionID, Cred, List) :-
	write('***************************************************'), nl,
	write('DEBUG: The credential to be shown is IDEMIX '), nl,
	write('***************************************************'), nl,
	Cred,
	usersCred(User, Cred),
	showCredAttrs(User, SP, TransactionID, Cred, List)
	.
	
	
/*{===============================================================
 |
 |  createListOfAttrNames(ListOfNames, ListOfAttrs)
 |    inputs: List of attribute structures 
 |    effects: extracts the attribute names from the list of 
 | 				attr(Name, Value) structures.
 |
 +=============================================================}*/


createListOfAttrNames([], []) :- !.

createListOfAttrNames([attr(NewFirst, _)|Rest], [NewFirst|NewRest]) :-
	write('DEBUG: Creating a list of attrs to show: '), write(NewFirst), nl, 
	createListOfAttrNames(Rest, NewRest).


	
/*{===============================================================
 |
 |  showCredAttrs(User, SP, TransactionID, Cred, AttrList)
 |    inputs: User, SP, TransactionID, Cred, AttrList 
 |    outputs: ---
 |    effects: calls the addToProvidersDB predicate for all the attributes from the given list 
 |
 +=============================================================}*/

 
showCredAttrs(_, _, _, _, []) :- !.


/*-------in case TID is given -------*/

showCredAttrs(User, SP, TransactionID, Cred, [First|Rest]) :-
	%write('DEBUG: Grounding attribute. '), nl,
	ground(TransactionID), !, 
	%write('DEBUG: TID Attribute grounded. '), nl,
	!,
	(
		transactionID(TransactionID), write('DEBUG: TransactionID exists. '), nl, !
	;
		assert(transactionID(TransactionID)), write('DEBUG: Asserted the new transactionID. '), nl
	),
	%write('DEBUG: Exited checking or asserting TransactionID. '), nl,
	getCredAttribute(Cred, First, AttrValue),
	%write('DEBUG: executed getting the cred attr'), nl,
	cred(_, Technology, Type, _),
	%write('DEBUG: determined the possible cred. '), nl,
	Cred = cred(_, Technology, Type, _),
	%write('DEBUG: assigned credential structure to Cred variable. '), nl,
	usersCred(User, Cred),
	%write('DEBUG: found the user credential. '), nl,
	%write('DEBUG: Important. About to add to DB: '), write(First), nl, 
	getCredAttribute(Cred, 'Issuer', Issuer),
	%
	write('After adding to providers DB, entering data in collaborating SPs databases.'), nl,
	write(addToProvidersDB(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer))), nl,
	addToProvidersDB(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	write(addToCollaboratingProvidersDBs(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer))), nl,
	addToCollaboratingProvidersDBs(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	%
	%write('DEBUG: Show remaining attrs: '), nl,
	%write(showCredAttrs(User, SP, TransactionID, Cred, Rest)), nl,
	showCredAttrs(User, SP, TransactionID, Cred, Rest)
	.

	
/*-------in case TID needs to be created--------*/
showCredAttrs(User, SP, TransactionID, Cred, [First|Rest]) :-
	write('DEBUG: Entered the second rule for showCredAttrs. '),
	startTransaction(User, SP, TransactionID),
	assert(transactionID(TransactionID)),
	getCredAttribute(Cred, First, AttrValue),
	cred(_, Technology, Type, _),
	Cred = cred(_, Technology, Type, _),
	usersCred(User, Cred),
	getCredAttribute(Cred, 'Issuer', Issuer),
	addToProvidersDB(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	write(addToProvidersDB(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer))), nl,
	addToCollaboratingProvidersDBs(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer)),
	write(addToCollaboratingProvidersDBs(SP, TransactionID, attr(First, AttrValue), credSource(Technology, Type, Issuer))), nl,
	showCredAttrs(User, SP, TransactionID, Cred, Rest)
	.
	

	
/*{===============================================================
 |
 |  userInput(User, SP, TransactionID, Attrs)
 |    inputs: User, SP, TransactionID, Attrs 
 |    outputs: ---
 |    effects: calls the addToProvidersDB predicate for all the given attributes 
 |
 +=============================================================}*/
	
userInput(_, _, _, []) :- !.
	
userInput(User, SP, TransactionID, [First|Rest]) :-
	write('DEBUG: recording the user input. '), nl, 
	addToProvidersDB(SP, TransactionID, First, 'UserInput'),
	write(addToProvidersDB(SP, TransactionID, First, 'UserInput')), nl,
	addToCollaboratingProvidersDBs(SP, TransactionID, First, 'UserInput'),
	write(addToCollaboratingProvidersDBs(SP, TransactionID, First, 'UserInput')), nl,
	userInput(User, SP, TransactionID, Rest)
	.

	
	
/*{+==============================================================
 |
 | addToProvidersDB(SP, TransactionID, Attribute, Source)
 |    inputs: SP, linked Arrtibute and TransactionID
 |    outputs: ---
 |    effects: creates a profile entry, a link between a transactionID and an attribute, with the indication of the source
 |
 +================================================================}*/


addToProvidersDB(SP, TransactionId, Attribute, Source):-
	write('------------------------------------------'), nl,
	write('IN THE addToProvidersDB'), nl, 
	write(addToProvidersDB(SP, TransactionId, Attribute, Source)), nl, 
		(
		dynamicProfile(SP, TransactionId, Attribute, Source), !
		;
		assert(dynamicProfile(SP, TransactionId, Attribute, Source)),
		write('DEBUG: Added to providers database: '), nl, 
		write(dynamicProfile(SP, TransactionId, Attribute, Source)), nl
		), 
	establishLinks(SP, TransactionId, Attribute, Source)
	.

	

/*{+==============================================================
 |
 | addToCollaboratingProvidersDBs(SP, TransactionID, Attribute, Source)
 |    inputs: SP, linked Arrtibute and TransactionID
 |    outputs: ---
 |    effects: creates a profile entry, a link between a transactionID and an attribute, with the indication of the source
 |			in the databases of the providers which collaborate with the provider SP		
 |
 +================================================================}*/

 %

	
/*{+==============================================================
 |
 | establishLinks(SP, TransactionId, Attribute, Source)
 |    inputs: SP, TransactionId, Attribute, Source
 |    outputs: ---
 |    effects: If Attribute is unique, searches for same attribute in other profiles and
 |              when found: establishes a link between the profiles
 |
 +================================================================}*/

establishLinks(SP, TransactionId, attr(AttrName, AttrValue), Source) :-	
	nl, write('------------------------------------------------------'), nl,
	write('DEBUG: Starting with profile links establishment'), nl, 
	write(establishLinks(SP, TransactionId, attr(AttrName, AttrValue), Source)), nl,
	write('------------------------------------------------------'), nl, nl,
	isUnique(AttrName, Source),
	write('DEBUG: Found unique attribute: '), write(isUnique(AttrName, Source)), nl, 
	write('DEBUG: In TID: '), write(TransactionId), nl, 
	transactionID(TransactionId2),
	write('DEBUG: Other considered TID: '), write(TransactionId2), nl, 
	\+ TransactionId = TransactionId2,
	write('DEBUG: Other considered TID is: '), write(TransactionId2), nl, 
	(
		(
		write('DEBUG: About to search for the attribute in the other considered profile. '), nl,
		write(attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source))), nl,
		attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source)),
		write(attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source))), nl,
		write('DEBUG: Found the required unique attribute in profile: '), nl,
		write(attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source))), nl, 
		Reason = reason(AttrName, Source)
		)
		;
		(
		collaboratingSPs(SP, SP2),
		%write('DEBUG: Found collaborating SPs: '), write(SP), write(' '), write(SP2), nl, 
		%write('DEBUG: About to look for the attr in other SPs profiles: '), nl,
		%write(attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source))), nl, 
		attrsInProfile(SP2, TransactionId2, attrNameSourcePair(attr(AttrName, AttrValue), Source)),
		%write('DEBUG: Found the attr in profile: '), write(TransactionId2), nl,
			(
			%Note - this disjunction can be extended when the list of possible types of sources is extended
				(
					Source = 'UserInput',
					atomic_list_concat(['collaboration', SP, '-', SP2, ':', 'UserInput'], Reason)
				)
				;
				(
					Source = credSource(Technology, Type, Issuer),  %???ADDED THE ISSUER
					atomic_list_concat(['collaboration', SP, '-', SP2, ':', 'credSource', '(', Technology, '; ', Type, ')'], Reason)
				)
			)
		)
	),	
	write('DEBUG: Going into formLink: '), write(formLink(SP, TransactionId, TransactionId2, reason(AttrName, Source))), nl, 
	formLink(SP, TransactionId, TransactionId2, Reason),
	formLink(SP2, TransactionId2, TransactionId, Reason),
	fail.
	

	
establishLinks(SP, TransactionId, Attribute, Source) :-
	write(establishLinks(SP, TransactionId, Attribute, Source)), nl, 
	write('DEBUG: Establishing the links for unique sets. '), nl,
	write(establishLinks(SP, TransactionId, Attribute, Source)), nl, 
	establishLinksBasedOnUniqueSets(SP, TransactionId),
	fail.
	

establishLinks(_, _, _, _).

	
	
/*{+==============================================================
 |
 | establishLinksBasedOnUniqueSets(SP, TransactionId)
 |    inputs: SP, TransactionId
 |    outputs: ---
 |    effects: checks for linked profiles based on unique sets of attributes
 |
 +================================================================}*/
 
establishLinksBasedOnUniqueSets(SP, TransactionId) :-
	write('DEBUG: entered establishLinksBasedOnUniqueSets. '), nl,
	uniqueSetInLinkedProfiles(SP, TransactionId, UniqueSet),
	write('DEBUG: Has a unique set: '), write(UniqueSet), nl,
	transactionID(TransactionId2),
	%\+ transactionId = transactionId2,
	\+ TransactionId = TransactionId2,
	\+ isLinked(SP, TransactionId, TransactionId2, _),
	write('DEBUG: Going to search for unique set in linked profiles (single SP): '), 
	write(uniqueSetInLinkedProfiles(SP2, TransactionId2, UniqueSet)), nl,
	collaboratingSPs(SP, SP2),
	uniqueSetInLinkedProfiles(SP2, TransactionId2, UniqueSet),
	write('DEBUG: Found uniqueSetInLinkedProfiles, about to formLink from (establishLinksBasedOnUniqueSets). '), nl,
	formLink(SP, TransactionId, TransactionId2, reason('UniqueSetInLinkedProfiles', UniqueSet)), 
	%
	formLink(SP2, TransactionId, TransactionId2, reason('UniqueSetInLinkedProfiles', UniqueSet)), 
	fail.
	
 
establishLinksBasedOnUniqueSets(_, _).


/*{+==============================================================
 |
 | establishLinksBasedOnUniqueSetsCollaboratingSPs(SP, TransactionId)
 |    inputs: SP, TransactionId
 |    outputs: ---
 |    effects: checks for linked profiles in merged databases of collaborating SPs, based on unique sets of attributes
 |
 |	NOTE: This rule is to be used if the data is not immediately exchanged between 
 |			the collaborating providers and recorded in the databases of the 
 |			collaborating providers. 
 |
 +================================================================}*/
 
 
establishLinksBasedOnUniqueSetsCollaboratingSPs(SP, TransactionId) :-
	write('DEBUG: entered establishLinksBasedOnUniqueSetsCollaboratingSPs. '), nl,
	uniqueSetInLinkedProfilesCollaboratingSPs(SP, TransactionId, UniqueSet),
	%write('DEBUG: Has a unique set: '), write(UniqueSet), nl,
	transactionID(TransactionId2),
	\+ TransactionId2 = TransactionId,
	\+ isLinked(SP, TransactionId, TransactionId2, _),
	collaboratingSPs(SP, SP2),
	write('DEBUG: Going to search for unique set in linked profiles for collaborating SPs for first TID: '), write(TransactionId), nl,
	write(establishLinksBasedOnUniqueSetsCollaboratingSPs(SP2, TransactionId2, UniqueSet)), nl,
	uniqueSetInLinkedProfilesCollaboratingSPs(SP2, TransactionId2, UniqueSet),
	formLink(SP, TransactionId, TransactionId2, reason('uniqueSetInLinkedProfilesCollaboratingSPs', UniqueSet)), 
	formLink(SP2, TransactionId, TransactionId2, reason('uniqueSetInLinkedProfilesCollaboratingSPs', UniqueSet)), 
	fail.
	

 
establishLinksBasedOnUniqueSetsCollaboratingSPs(_, _).
 

	
/*{+==============================================================
 |
 | linksBasedOnUniqueSetsInSingleProfile(SP, TransactionId)
 |    inputs: SP, TransactionId
 |    outputs: ---
 |    effects: checks for linked profiles based on unique sets of attributes
 |
 +================================================================}*/
 
linksBasedOnUniqueSetsInSingleProfile(SP, TransactionId) :-
	write('DEBUG: entered linksBasedOnUniqueSetsInSingleProfile. '), nl,
	hasUniqueSet(SP, TransactionId, UniqueSet),
	write('DEBUG: Has a unique set: '), write(UniqueSet), nl,
	transactionID(TransactionId2),
	\+ TransactionId = TransactionId2,
	\+ isLinked(SP, TransactionId, TransactionId2),
	hasUniqueSet(SP, TransactionId2, UniqueSet),
	formLink(SP, TransactionId, TransactionId2, reason('UniqueSet', UniqueSet)), 
	fail.
 
linksBasedOnUniqueSetsInSingleProfile(_, _).


/*{+==============================================================
 |
 | 	uniqueSetInLinkedProfiles(SP, TransactionId, UniqueSet)
 |    inputs: SP, TransactionId
 |    outputs: UniqueSet
 |    effects: finds the unique sets that linked profiles have
 |
 +================================================================}*/
 
uniqueSetInLinkedProfiles(SP, TransactionId, UniqueSet) :-
	write('DEBUG: Looking for unique set in profile: '), write(TransactionId), nl,
	uniqueSet(UniqueSet), 
	write('DEBUG: Found a unique set: '), write(UniqueSet), nl, 
	write(attrsInLinkedProfiles(SP, TransactionId, UniqueSet)), nl,
	attrsInLinkedProfiles(SP, TransactionId, UniqueSet),
	write('Found the unique set for: '), nl,
	write(attrsInLinkedProfiles(SP, TransactionId, UniqueSet)), nl
	.
	
	
/*{+==============================================================
 |
 | 	uniqueSetInLinkedProfilescollaboratingSPs(SP, TransactionId, UniqueSet)
 |    inputs: SP, TransactionId
 |    outputs: UniqueSet
 |    effects: finds the unique sets that linked profiles have
 |
 +================================================================}*/
 
	
uniqueSetInLinkedProfilesCollaboratingSPs(SP, TransactionId, UniqueSet) :-
	write('DEBUG: (uniqueSetInLinkedProfilesCollaboratingSPs) Looking for unique set in profile: '), write(TransactionId), nl,
	uniqueSet(UniqueSet), 
	write('DEBUG: Found a unique set: '), write(UniqueSet), nl, 
	attrsInLinkedProfilesCollaboratingSPs(SP, TransactionId, UniqueSet)
	.
	

/*{+==============================================================
 |
 | 	hasUniqueSet(SP, TransactionId, UniqueSet)
 |    inputs: SP, TransactionId
 |    outputs: UniqueSet
 |    effects: finds the unique sets that a profile has
 |
 +================================================================}*/
 
hasUniqueSet(SP, TransactionId, UniqueSet) :-
	write('DEBUG: (hasUniqueSet) Looking for unique set in profile: '), write(TransactionId), nl,
	uniqueSet(UniqueSet), 
	write('DEBUG: Found a unique set: '), write(UniqueSet), nl, 
	attrsInProfile(SP, TransactionId, UniqueSet)
	.
	

/*{+==============================================================
 |
 | 	attrsInProfile(SP, TransactionId, Set)
 |    inputs: SP, TransactionId, Set
 |    outputs: ---
 |    effects: checks if the set of attributes exists in the profile
 |
 +================================================================}*/

attrsInProfile(_, _, []) :- !.

attrsInProfile(SP, TransactionId, attrNameSourcePair(Attribute, Source)) :-
	write('DEBUG: looking for attrsInProfile: '), write(Attribute), nl,
	write('DEBUG: With: '), write(profile(SP, TransactionId, Attribute, Source)), nl, 
	profile(SP, TransactionId, Attribute, Source),
	write('DEBUG: Within attrsInProfile, found a profile with the attribute: '), nl, 
	write(profile(SP, TransactionId, Attribute, Source)), nl,
	!.
	
attrsInProfile(SP, TransactionId, [attrNameSourcePair(Attribute, Source)|Rest]) :-
	write('DEBUG: looking for attrsInProfile: '), write(Attribute), nl,
	write('DEBUG: With: '), write(profile(SP, TransactionId, Attribute, Source)), nl, 
	profile(SP, TransactionId, Attribute, Source),
	write('DEBUG: Within attrsInProfile, found a profile with the attribute: '), nl, 
	write(profile(SP, TransactionId, Attribute, Source)), nl,
	attrsInProfile(SP, TransactionId, Rest).

	
/*{+==============================================================
 |
 | 	attrsInLinkedProfiles(SP, TransactionId, Set)
 |    inputs: SP, TransactionId, Set
 |    outputs: ---
 |    effects: checks if the set of attributes exists in the profile
 |
 +================================================================}*/

attrsInLinkedProfiles(_, _, []) :- !.
	
attrsInLinkedProfiles(SP, TransactionId, [attrNameSourcePair(Attribute, Source)|Rest]) :-
	write('DEBUG: looking for attrsInLinkedProfiles: '), write(Attribute), nl,
	nl,nl,write(recordedInDB(SP, TransactionId, attr(Attribute, _), Source)),nl,nl,
	recordedInDB(SP, TransactionId, attr(Attribute, _), Source),
	write('DEBUG: Found the attribute recorded in the DB: '), write(TransactionId), nl,
	attrsInLinkedProfiles(SP, TransactionId, Rest).	
	
/*{+==============================================================
 |
 | 	attrsInLinkedProfilesCollaboratingSPs(SP, TransactionId, Set)
 |    inputs: SP, TransactionId, Set
 |    outputs: ---
 |    effects: checks if the set of attributes exists in linked profiles kept by collaborating SPs
 |
 +================================================================}*/

attrsInLinkedProfilesCollaboratingSPs(_, _, []) :- !.
	
attrsInLinkedProfilesCollaboratingSPs(SP, TransactionId, [attrNameSourcePair(Attribute, Source)|Rest]) :-
	write('DEBUG: looking for attrInProfile: '), write(Attribute), nl,
	recordedInDBCollaboratingSPs(SP, TransactionId, attr(Attribute, _), Source),
	write('DEBUG: Found the attribute recorded in the DB of collaborating SPs: '), write(TransactionId), nl,
	attrsInLinkedProfilesCollaboratingSPs(SP, TransactionId, Rest).	
	
/*{+==============================================================
 |
 | formLink(SP, TransactionId1, TransactionId2, Reason)
 |    inputs: SP, TransactionId1, TransactionId2, Reason
 |    outputs: ---
 |    effects: Extends the list of reasons for the link or creates a link between the transactions' IDs
 |
 +================================================================}*/
 
formLink(SP, TransactionId, TransactionId2, Reason) :-
	write('DEBUG: In the formLink with: '), write(formLink(SP, TransactionId, TransactionId2, Reason)), nl,
	isLinked(SP, TransactionId, TransactionId2, Reasons),
	write('DEBUG: isLinked succeeded!!!! '), nl,
	!,
	(	
		member(Reason, Reasons),
		write('DEBUG: The reason is also noted already!'), nl,
		!
	;
		write('DEBUG: Going to extend the linking reason. '), nl, 
		extendLinkingReason(SP, TransactionId, TransactionId2, Reason, Reasons)
	),
	fail.
	

formLink(SP, TransactionId, TransactionId2, Reason) :-
	write('DEBUG: Trying the second branch of form link'), nl,
	!,
	assert(isLinked(SP, TransactionId, TransactionId2, [Reason])),
	assert(isLinked(SP, TransactionId2, TransactionId, [Reason]))
	.
	
formLink(_, _, _, _).

establishSuperLinks(SP, TransactionId, TransactionId2) :-
	isLinked(SP, TransactionId, TransactionId3, Reason2),
	\+ TransactionId = TransactionId3,
	\+ TransactionId2 = TransactionId3,
	write('DEBUG: In (establishSuperLinks), found isLinked:'), nl,
	write(isLinked(SP, TransactionId, TransactionId3, Reason2)), nl, 
	write('DEBUG: About to formLink with SuperLinking: '), nl,
	write(formLink(SP, TransactionId3, TransactionId2, 'SuperLinking')), nl,
	formLink(SP, TransactionId3, TransactionId2, 'SuperLinking'),
	formLink(SP, TransactionId2, TransactionId3, 'SuperLinking').

establishSuperLinks(SP, TransactionId, TransactionId2) :-
	isLinked(SP, TransactionId2, TransactionId3, Reason3),
	\+ TransactionId = TransactionId3,
	\+ TransactionId2 = TransactionId3,
	write('DEBUG: In (establishSuperLinks), found isLinked:'), nl,
	write(isLinked(SP, TransactionId, TransactionId3, Reason2)), nl, 
	write('DEBUG: About to formLink with SuperLinking: '), nl,
	write(formLink(SP, TransactionId3, TransactionId, 'SuperLinking')), nl,
	formLink(SP, TransactionId3, TransactionId, 'SuperLinking'),
	formLink(SP, TransactionId, TransactionId3, 'SuperLinking').
	
establishSuperLinks(SP, TransactionId, TransactionId2) :-
		establishLinksBasedOnUniqueSets(SP, TransactionId),
		establishLinksBasedOnUniqueSets(SP, TransactionId2).

/*{+==============================================================
 |
 | recordedInDB(SP, TransactionId, Attribute, Source)
 |    inputs: SP, TransactionId, Attribute, Source
 |    outputs: ---
 |    effects: checking whether a TransactionId is linked to an attribute
 |
 +================================================================}*/
	

recordedInDB(SP, TransactionId, Attribute, Source):-
	write('In recorded in DB: '), nl, nl,
	write(profile(SP, TransactionId, Attribute, Source)), nl, nl, 
	profile(SP, TransactionId, Attribute, Source),
	write('It succeeded. '), nl, nl
	. 

recordedInDB(SP, TransactionId, Attribute, Source):- 
	isLinked(_, TransactionId, TransactionId2, Reason),
	profile(SP, TransactionId2, Attribute, Source)
	.
	
	
	
/*{+==============================================================
 |
 | recordedInDBCollaboratingSPs(SP, TransactionId, Attribute, Source)
 |    inputs: SP, TransactionId, Attribute, Source
 |    outputs: ---
 |    effects: checking whether a TransactionId is linked to an attribute in merged DBs
 |
 |	NOTE: This is to be used when the disclosed data is not immediately recorded in the collaborating
 |			provider's databases.
 |
 +================================================================}*/
	

recordedInDBCollaboratingSPs(SP, TransactionId, Attribute, Source):-
	profile(SP, TransactionId, Attribute, Source), !. 

recordedInDBCollaboratingSPs(SP, TransactionId, Attribute, Source):- 
	isLinked(SP, TransactionId, TransactionId2, Reason),
	profile(SP, TransactionId2, Attribute, Source), !.
	
recordedInDBCollaboratingSPs(SP, TransactionId, Attribute, Source):- 
	collaboratingSPs(SP, SP2),
	(
		profile(SP2, TransactionId2, Attribute, Source), !
		;
		isLinked(SP2, TransactionId2, TransactionId3, Reason),
		profile(SP, TransactionId2, Attribute, Source), !
	)
	.

/*{+==============================================================
 |
 | extendLinkingReason(SP, TransactionId, TransactionId2, Source, Reason, Reasons)
 |    inputs: SP, TransactionId, TransactionId2, Source, Reason, Reasons
 |    outputs: ---
 |    effects: Extends the list of reasons for the link (when necessary)
 |
 +================================================================}*/


extendLinkingReason(SP, TransactionId, TransactionId2, Reason, Reasons) :- 
	member(Reason, Reasons),
	!.

extendLinkingReason(SP, TransactionId, TransactionId2, Reason, Reasons) :-
	retract(isLinked(SP, TransactionId, TransactionId2, Reasons)),
	retract(isLinked(SP, TransactionId2, TransactionId, Reasons)),
	assert(isLinked(SP, TransactionId, TransactionId2, [Reason | Reasons])),
	assert(isLinked(SP, TransactionId2, TransactionId, [Reason | Reasons])).




