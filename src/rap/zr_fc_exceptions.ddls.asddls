@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZFC_EXCEPTIONS'
define root view entity ZR_FC_EXCEPTIONS
  as select from zfc_exceptions
{
  key ident as Ident,
  key jahr as Jahr,
  key von as Von,
  bis as Bis,
  ltext as Text,
  @Semantics.user.createdBy: true
  local_created_by as LocalCreatedBy,
  @Semantics.systemDateTime.createdAt: true
  local_created_at as LocalCreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  local_last_changed_by as LocalLastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed_at as LocalLastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt
  
}
