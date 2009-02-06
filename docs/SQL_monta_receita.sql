SELECT tbl_livros_receitas.ds_livro_receita as Livro,
       tbl_receita.ds_receita as Receita,
       tbl_tags_receita.ds_tagname as TagName,
       tbl_item_receita.vl_number as ValorNumerico,
       tbl_item_receita.vl_string as ValorTexto
from tbl_livros_receitas
left join tbl_receita on (tbl_receita.cd_livro_receita=tbl_livros_receitas.id_livro_receita)
left join tbl_tags_receita on (tbl_tags_receita.cd_livro_receita=tbl_livros_receitas.id_livro_receita)
left join tbl_item_receita on (tbl_item_receita.cd_tag_receita=tbl_tags_receita.id_tag_receita AND tbl_item_receita.cd_receita=tbl_receita.id_receita)
where tbl_receita.cd_livro_receita=1 and tbl_receita.id_receita=1;