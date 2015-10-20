/*{+==============================================================
 |
 | addToCollaboratingProvidersDBs(SP, TransactionID, Attribute, Source)
 |    inputs: SP, linked Arrtibute and TransactionID
 |    outputs: ---
 |    effects: creates a profile entry, a link between a transactionID and an attribute, with the indication of the source
 |			in the databases of the providers which collaborate with the provider SP		
 |
 +================================================================}*/



addToCollaboratingProvidersDBs(SP, TransactionId, Attribute, Source):-
write('------------------------------------------'), nl,
write('IN THE addToCollaboratingProvidersDBs'), nl, 
write(addToCollaboratingProvidersDBs(SP, TransactionId, Attribute, Source)), nl, 
	collaboratingSPs(SP, SP1),
	write('Analysing the DB of collaborating SP: '), write(SP1), nl,
	(
		dynamicProfile(SP1, TransactionId, Attribute, Source), !
	;
		assert(dynamicProfile(SP1, TransactionId, Attribute, Source)),
		write('DEBUG: Added to providers database: '), nl, 
		write(dynamicProfile(SP1, TransactionId, Attribute, Source)), nl
	), 
	% this is optional -- depends on the scenario that is modelled -- if the providers would share the knowledge on existing links
	%establishLinks(SP1, TransactionId, Attribute, Source),
	fail.
	
	
addToCollaboratingProvidersDBs(_, _, _, _).

