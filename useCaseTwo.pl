%:- tell('UseCaseTwoOUTPUT.txt').
:- [modelBase].
:- [model].
:-[addingToCollaboratorsDB].

:- write('DEBUG: university test').

:-defineCredType('X509', 'BeID', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
								attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).
								
:-defineCredType('X509', 'BeDL', [attrDef('Subject', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique'),
								attrDef('VehicleClass', 'Letter', 'Known', 'Assigned', 'Shared')]).
								
:-defineCredType('Idemix', 'loyalty credential', [attrDef('Id', 'String', 'Known', 'Chosen', 'Unique'),
	                           attrDef('Rand', 'Integer', 'Chosen', 'Hidden', 'Shared'),
                              attrDef('SysRand', 'Integer', 'Known', 'Chosen', 'Shared'),
                              attrDef('Gender', 'Letter', 'Known', 'Assigned', 'Shared'),
                              attrDef('ZIP', 'ZipCode', 'Known', 'Assigned', 'Shared'),
                              attrDef('YearOfBirth', 'Year', 'Known', 'Assigned', 'Shared')
                             ]).
							 
:-defineCredType('X509', 'bank card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
									attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
									attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).

							 						
:-defineCredType('X509', 'student card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('StudentNumber', 'Integer', 'Known', 'Assigned', 'Unique'),
									attrDef('University', 'String', 'Known', 'Assigned', 'Shared')]).

									
									
:-defineCredType('Idemix', 'TTPID', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
								attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).
								

:-defineCredType('X509', 'ShopperCard', [attrDef('ZIP', 'Integer', 'Known', 'Assigned', 'Shared'),
								attrDef('ShopperNumber', 'Integer', 'Chosen', 'Known', 'Unique')]).
																
			



%:-assert(collaboratingSPs('Provider1', 'AdsProvider')).
%:-assert(collaboratingSPs('Provider1', 'Provider2')).
%:-assert(collaboratingSPs('Provider2', 'Provider1')).

	
:- issueCred('Alice', 'My shopper card1', 'X509', 'ShopperCard', 'Provider1', '2015', [attr('ZIP', '3000'), 
						attr('ShopperNumber', '12345')], CredSP1).

:- issueCred('Alice', 'My shopper card2', 'X509', 'ShopperCard', 'Provider2', '2015', [attr('ZIP', '3000'), 
						attr('ShopperNumber', '12345')], CredSP2).	
	
:- issueCred('Alice', 'My Belgian ID card', 'X509', 'BeID', 'gov', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('Address', 'Leuven'), 
						attr('DoB', '1980'), attr('PK', '12345')], Cred).


:- issueCred('Alice', 'My TTP ID card', 'Idemix', 'TTPID', 'gov', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('Address', 'Leuven'), 
						attr('DoB', '1980'), attr('PK', '12345')], Cred1).


:- issueCred('Alice', 'MyStudentCard', 'X509', 'student card', 'KU Leuven', '2015', [attr('Name', 'Jane'), 
							attr('Surname', 'Johnson'), attr('StudentNumber', '1234'), 
							attr('University', 'KU Leuven')], Cred2).


:- showCred('Alice', 'Provider1', '1', cred('My TTP ID card', _, _, _), ['DoB']).
:- showCred('Alice', 'Provider1', '1', cred('My shopper card1', _, _, _), ['ShopperNumber']).
:- showCred('Alice', 'Provider2', '2', cred('My shopper card2', _, _, _), ['ShopperNumber']).


:- showCred('Alice', 'Provider2', '2', cred('My Belgian ID card', _, _, _), ['DoB']).


:- userInput('Alice', 'Provider2', '2', ['AddressAlice']).
:- userInput('Alice', 'Provider1', '1', ['AddressAlice']).
						


:- nl, nl, write('-------------------------------------------------'), nl, write('FINISHED'), nl, 
	write('---------------------------------------------------').

	
	
/*------------------------------------------
%
%		Queries for creating the results:
%
%		profile('Provider1', B, C, D).
%
%		profile('Provider2', B, C, D).
%
%		profile(A, B, C, D).
%
------------------------------------------*/	

	
	
