:- [model_GitHub].
:- [modelBase_GitHub].
:- [servicePolicy].
:-[addingToCollaboratorsDB].
:- [disclosureWeight].


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
									attrDef('University', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).									


									
:-defineCredType('Idemix', 'eID', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
								attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).									
			
			
:-assert(uniqueSet([attrNameSourcePair('Name', credSource('X.509', 'BeID', 'gov')), attrNameSourcePair('Surname', credSource('X.509', 'BeID', 'gov'))])).					

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

:- issueCred('Alice', 'My TTP ID card', 'Idemix', 'eID', 'TTP', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('Address', 'Leuven'), 
						attr('DoB', '1980'), attr('PK', '98765')], Cred1).
						
:- issueCred('Alice', 'MyStudentCard', 'X.509', 'student card', 'KU Leuven', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('StudentNumber', '1234'), 
						attr('University', 'KU Leuven'), attr('PK', '111')], Cred2).

				
% Template is:		
%:- assert(attributeWeight(Type, Technology, Issuer, AttributeName, Weight)).
		

:- assert(attributeWeight('BeID', 'X.509', 'gov', 'IssuerSig', 0)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'Issuer', 0)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'Name', 5)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'Surname', 8)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'Address', 3)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'DoB', 12)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'PK', 15)).
:- assert(attributeWeight('BeID', 'X.509', 'gov', 'ExpiryDate', 0)).

:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'IssuerSig', 0)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'Issuer', 0)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'Name', 5)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'Surname', 8)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'Address', 3)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'DoB', 12)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'PK', 15)).
:- assert(attributeWeight('eID', 'Idemix', 'TTP', 'ExpiryDate', 0)).

:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'IssuerSig', 0)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'Issuer', 0)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'Name', 5)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'Surname', 7)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'StudentNumber', 13)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'University', 3)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'PK', 15)).
:- assert(attributeWeight('student card', 'X.509', 'KU Leuven', 'ExpiryDate', 0)).


% The following examples can be checked:
%:- showCredCount('Alice', 'OnlineShop', '1', cred('My Belgian ID card', _, _, _), ['DoB']).

%:- showCredCount('Alice', 'OnlineShop', '100', cred('MyStudentCard', A, B, C), ['Name']).

:- showCredCount('Alice', 'OnlineShop', '10', cred('My TTP ID card', A, B, C), ['Name', 'Surname', 'DoB']).




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