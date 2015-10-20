:- [model_GitHub].
:- [modelBase_GitHub].
:- [servicePolicy].
:-[addingToCollaboratorsDB].


:-defineCredType('X.509', 'BeID', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
								attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).
								
:-defineCredType('X.509', 'BeDL', [attrDef('Subject', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique'),
								attrDef('VehicleClass', 'Letter', 'Known', 'Assigned', 'Shared')]).
								
:-defineCredType('Idemix', 'loyalty credential', [attrDef('Id', 'String', 'Known', 'Chosen', 'Unique'),
	                           attrDef('Rand', 'Integer', 'Chosen', 'Hidden', 'Shared'),
                              attrDef('SysRand', 'Integer', 'Known', 'Chosen', 'Shared'),
                              attrDef('Gender', 'Letter', 'Known', 'Assigned', 'Shared'),
                              attrDef('ZIP', 'ZipCode', 'Known', 'Assigned', 'Shared'),
                              attrDef('YearOfBirth', 'Year', 'Known', 'Assigned', 'Shared')
                             ]).
							 
:-defineCredType('X.509', 'bank card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
									attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
									attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).

							 
/*:-defineCredType('Idemix', 'loyalty credential', [attrDef('Id', 'String', 'Known', 'Chosen', 'Unique'),
	                           attrDef('Rand', 'Integer', 'Chosen', 'Hidden', 'Shared'),
                              attrDef('SysRand', 'Integer', 'Known', 'Chosen', 'Shared'),
                              attrDef('Gender', 'Letter', 'Known', 'Assigned', 'Shared'),
                              attrDef('ZIP', 'ZipCode', 'Known', 'Assigned', 'Shared'),
                              attrDef('YearOfBirth', 'Year', 'Known', 'Assigned', 'Shared')
                             ]).
	*/						
	
:-defineCredType('X.509', 'student card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('StudentNumber', 'Integer', 'Known', 'Assigned', 'Unique'),
									attrDef('University', 'String', 'Known', 'Assigned', 'Shared')]).

			

:- assert(isUnique('PK', credSource('X.509', 'student card', 'KU Leuven'))).
:- assert(isUnique('PK', credSource('X.509', 'BeID', 'gov'))).
			
:-assert(uniqueSet([attrNameSourcePair('StudentNumber', credSource('X.509', 'student card', 'KU Leuven')), attrNameSourcePair('DoB', credSource('X.509', 'BeID', 'gov'))])).					


/*Providers' collaboration*/
:-assert(collaboratingSPs('AdsProvider', 'OnlineShop')).
:-assert(collaboratingSPs('OnlineShop', 'AdsProvider')).


/*Existing profile entries*/
:- 	assert(transactionID('99')).
:- assert(dynamicProfile('AdsProvider', '99', attr('StudentNumber', '1234'), credSource('X.509', 'student card', 'KU Leuven'))).
:- assert(dynamicProfile('AdsProvider', '99', attr('DoB', '1980'), credSource('X.509', 'BeID', 'gov'))).


	
	
	
:- issueCred('Alice', 'My Belgian ID card', 'X.509', 'BeID', 'gov', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('Address', 'Leuven'), 
						attr('DoB', '1980'), attr('PK', '12345')], Cred).


:- issueCred('Alice', 'MyStudentCard', 'X.509', 'student card', 'KU Leuven', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('StudentNumber', '1234'), 
						attr('University', 'KU Leuven')], Cred1).

				

	
	
		
		
:- assert(authPolicy('OnlineShop', 'purchaseGeneral', [attrAndSource(cred(_, 'X.509', 'BeID', _), ['DoB']), 
		attrAndSource(cred('MyStudentCard', _, _, _), ['Ownership']),
		attrAndSource('userInput', ['purchase'])
		])).

:- reqService('Alice', 'OnlineShop', '2', 'purchaseGeneral').

:- reqService('Alice', 'OnlineShop', '3', 'purchaseGeneral').
	



:- nl, nl, write('-------------------------------------------------'), nl, write('FINISHED'), nl, 
	write('---------------------------------------------------').

	
/*------------------------------------------
%
%		Queries for creating the results:
%
%		isLinked(A, B, C, D).
%
%		profile(A, B, C, D).
%
------------------------------------------*/