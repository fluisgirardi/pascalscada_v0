SELECT DISTINCT grupo.ds_groupname, usuario.ds_login, objeto.ds_objeto
FROM usuario
left join integrante_grupo on (usuario.id_usuario=integrante_grupo.id_usuario)
left join permissao_grupo on (integrante_grupo.id_grupo=permissao_grupo.id_grupo)
left join permissao_usuario on (usuario.id_usuario=permissao_usuario.id_usuario)
left join objeto on (objeto.id_objeto = permissao_usuario.id_objeto OR
                     objeto.id_objeto = permissao_grupo.id_objeto)
left join grupo on (usuario.id_usuario=integrante_grupo.id_usuario AND
                    grupo.id_grupo=integrante_grupo.id_grupo and
                    integrante_grupo.id_grupo=permissao_grupo.id_grupo and
                    objeto.id_objeto = permissao_grupo.id_objeto)

WHERE usuario.ds_login="fabio"
order by grupo.ds_groupname, usuario.ds_login