 SELECT 
    tbum_user.ds_login,
    NULL AS ds_groupname,  
    tbum_object.ds_object
  FROM
    tbum_user, 
    tbum_object, 
    tbum_permission
  WHERE 
    tbum_user.id_user = tbum_permission.cd_user AND
    tbum_object.id_object = tbum_permission.cd_object
 
UNION
  
  SELECT 
    tbum_user.ds_login, 
    tbum_group.ds_groupname, 
    tbum_object.ds_object
  FROM 
    tbum_user, 
    tbum_group_member, 
    tbum_permission, 
    tbum_object, 
    tbum_group
  WHERE 
    tbum_user.id_user = tbum_group_member.cd_user AND
    tbum_group_member.cd_group = tbum_permission.cd_group AND
    tbum_group_member.cd_group = tbum_group.id_group AND
    tbum_permission.cd_object = tbum_object.id_object