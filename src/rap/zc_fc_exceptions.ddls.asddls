@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_FC_EXCEPTIONS'
@ObjectModel.semanticKey: [ 'Ident', 'Jahr', 'Von' ]
define root view entity ZC_FC_EXCEPTIONS
  provider contract transactional_query
  as projection on ZR_FC_EXCEPTIONS
{
  key Ident,
  key Jahr,
  key Von,
  Bis,
  Text,
  LocalLastChangedAt
  
}
