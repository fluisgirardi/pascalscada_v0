-- MySQL Administrator dump 1.4
--
-- ------------------------------------------------------
-- Server version	5.0.19-nt


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


--
-- Create schema test
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ test;
USE test;

--
-- Table structure for table `test`.`grupo`
--

DROP TABLE IF EXISTS `grupo`;
CREATE TABLE `grupo` (
  `id_grupo` int(10) unsigned NOT NULL auto_increment,
  `ds_groupname` varchar(50) NOT NULL,
  `ds_groupdesc` varchar(200) default NULL,
  PRIMARY KEY  (`id_grupo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`grupo`
--

/*!40000 ALTER TABLE `grupo` DISABLE KEYS */;
INSERT INTO `grupo` (`id_grupo`,`ds_groupname`,`ds_groupdesc`) VALUES 
 (1,'admins',NULL),
 (2,'view',NULL),
 (3,'users',NULL);
/*!40000 ALTER TABLE `grupo` ENABLE KEYS */;


--
-- Table structure for table `test`.`integrante_grupo`
--

DROP TABLE IF EXISTS `integrante_grupo`;
CREATE TABLE `integrante_grupo` (
  `id_usuario` int(10) unsigned NOT NULL,
  `id_grupo` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`id_usuario`,`id_grupo`),
  KEY `INTEGRANTE_GRUPO_FKIndex1` (`id_usuario`),
  KEY `INTEGRANTE_GRUPO_FKIndex2` (`id_grupo`),
  CONSTRAINT `integrante_grupo_ibfk_1` FOREIGN KEY (`id_usuario`) REFERENCES `usuario` (`id_usuario`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `integrante_grupo_ibfk_2` FOREIGN KEY (`id_grupo`) REFERENCES `grupo` (`id_grupo`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`integrante_grupo`
--

/*!40000 ALTER TABLE `integrante_grupo` DISABLE KEYS */;
INSERT INTO `integrante_grupo` (`id_usuario`,`id_grupo`) VALUES 
 (1,1),
 (1,2),
 (1,3),
 (2,1),
 (2,2),
 (2,3),
 (3,3),
 (4,2);
/*!40000 ALTER TABLE `integrante_grupo` ENABLE KEYS */;


--
-- Table structure for table `test`.`objeto`
--

DROP TABLE IF EXISTS `objeto`;
CREATE TABLE `objeto` (
  `id_objeto` int(10) unsigned NOT NULL auto_increment,
  `ds_objeto` varchar(50) NOT NULL,
  `ds_Descricao` varchar(255) NOT NULL,
  PRIMARY KEY  (`id_objeto`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`objeto`
--

/*!40000 ALTER TABLE `objeto` DISABLE KEYS */;
INSERT INTO `objeto` (`id_objeto`,`ds_objeto`,`ds_Descricao`) VALUES 
 (1,'SET','0'),
 (2,'GET','0'),
 (3,'OPEN','0'),
 (4,'CLOSE','0'),
 (5,'DEL','0'),
 (6,'CREATE','0');
/*!40000 ALTER TABLE `objeto` ENABLE KEYS */;


--
-- Table structure for table `test`.`permissao_grupo`
--

DROP TABLE IF EXISTS `permissao_grupo`;
CREATE TABLE `permissao_grupo` (
  `id_grupo` int(10) unsigned NOT NULL,
  `id_objeto` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`id_grupo`,`id_objeto`),
  KEY `PERMISSAO_GRUPO_FKIndex1` (`id_grupo`),
  KEY `PERMISSAO_GRUPO_FKIndex2` (`id_objeto`),
  CONSTRAINT `permissao_grupo_ibfk_1` FOREIGN KEY (`id_grupo`) REFERENCES `grupo` (`id_grupo`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `permissao_grupo_ibfk_2` FOREIGN KEY (`id_objeto`) REFERENCES `objeto` (`id_objeto`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`permissao_grupo`
--

/*!40000 ALTER TABLE `permissao_grupo` DISABLE KEYS */;
INSERT INTO `permissao_grupo` (`id_grupo`,`id_objeto`) VALUES 
 (1,1),
 (1,2),
 (1,3),
 (1,4),
 (1,5),
 (1,6),
 (2,2),
 (2,3);
/*!40000 ALTER TABLE `permissao_grupo` ENABLE KEYS */;


--
-- Table structure for table `test`.`permissao_usuario`
--

DROP TABLE IF EXISTS `permissao_usuario`;
CREATE TABLE `permissao_usuario` (
  `id_objeto` int(10) unsigned NOT NULL,
  `id_usuario` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`id_objeto`,`id_usuario`),
  KEY `PERMISSOES_FKIndex3` (`id_objeto`),
  KEY `id_usuario` (`id_usuario`),
  CONSTRAINT `permissao_usuario_ibfk_1` FOREIGN KEY (`id_usuario`) REFERENCES `usuario` (`id_usuario`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `permissao_usuario_ibfk_2` FOREIGN KEY (`id_objeto`) REFERENCES `objeto` (`id_objeto`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`permissao_usuario`
--

/*!40000 ALTER TABLE `permissao_usuario` DISABLE KEYS */;
INSERT INTO `permissao_usuario` (`id_objeto`,`id_usuario`) VALUES 
 (1,1),
 (2,1),
 (3,1),
 (4,1),
 (5,1),
 (6,1),
 (6,4);
/*!40000 ALTER TABLE `permissao_usuario` ENABLE KEYS */;


--
-- Table structure for table `test`.`tbl_item_receita`
--

DROP TABLE IF EXISTS `tbl_item_receita`;
CREATE TABLE `tbl_item_receita` (
  `cd_receita` int(10) unsigned NOT NULL,
  `cd_livro_receita` int(10) unsigned NOT NULL,
  `cd_tag_receita` int(10) unsigned NOT NULL,
  `vl_number` double default NULL,
  `vl_string` varchar(255) default NULL,
  PRIMARY KEY  (`cd_receita`,`cd_livro_receita`,`cd_tag_receita`),
  KEY `tbl_item_receita_FKIndex1` (`cd_receita`,`cd_livro_receita`),
  KEY `tbl_item_receita_FKIndex2` (`cd_tag_receita`,`cd_livro_receita`),
  CONSTRAINT `tbl_item_receita_ibfk_1` FOREIGN KEY (`cd_receita`, `cd_livro_receita`) REFERENCES `tbl_receita` (`id_receita`, `cd_livro_receita`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `tbl_item_receita_ibfk_2` FOREIGN KEY (`cd_tag_receita`, `cd_livro_receita`) REFERENCES `tbl_tags_receita` (`id_tag_receita`, `cd_livro_receita`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`tbl_item_receita`
--

/*!40000 ALTER TABLE `tbl_item_receita` DISABLE KEYS */;
INSERT INTO `tbl_item_receita` (`cd_receita`,`cd_livro_receita`,`cd_tag_receita`,`vl_number`,`vl_string`) VALUES 
 (1,1,2,33,NULL),
 (1,1,3,321,NULL);
/*!40000 ALTER TABLE `tbl_item_receita` ENABLE KEYS */;


--
-- Table structure for table `test`.`tbl_livros_receitas`
--

DROP TABLE IF EXISTS `tbl_livros_receitas`;
CREATE TABLE `tbl_livros_receitas` (
  `id_livro_receita` int(10) unsigned NOT NULL auto_increment,
  `ds_livro_receita` varchar(50) default NULL,
  PRIMARY KEY  (`id_livro_receita`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`tbl_livros_receitas`
--

/*!40000 ALTER TABLE `tbl_livros_receitas` DISABLE KEYS */;
INSERT INTO `tbl_livros_receitas` (`id_livro_receita`,`ds_livro_receita`) VALUES 
 (1,'livro 1'),
 (3,'livro 3');
/*!40000 ALTER TABLE `tbl_livros_receitas` ENABLE KEYS */;


--
-- Table structure for table `test`.`tbl_receita`
--

DROP TABLE IF EXISTS `tbl_receita`;
CREATE TABLE `tbl_receita` (
  `id_receita` int(10) unsigned NOT NULL auto_increment,
  `cd_livro_receita` int(10) unsigned NOT NULL,
  `ds_receita` varchar(255) default NULL,
  PRIMARY KEY  (`id_receita`,`cd_livro_receita`),
  KEY `tbl_receita_FKIndex1` (`cd_livro_receita`),
  CONSTRAINT `tbl_receita_ibfk_1` FOREIGN KEY (`cd_livro_receita`) REFERENCES `tbl_livros_receitas` (`id_livro_receita`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`tbl_receita`
--

/*!40000 ALTER TABLE `tbl_receita` DISABLE KEYS */;
INSERT INTO `tbl_receita` (`id_receita`,`cd_livro_receita`,`ds_receita`) VALUES 
 (1,1,'receita 1 L1'),
 (2,1,'receita 2 L1'),
 (3,1,'receita 3 L1');
/*!40000 ALTER TABLE `tbl_receita` ENABLE KEYS */;


--
-- Table structure for table `test`.`tbl_tags_receita`
--

DROP TABLE IF EXISTS `tbl_tags_receita`;
CREATE TABLE `tbl_tags_receita` (
  `id_tag_receita` int(10) unsigned NOT NULL auto_increment,
  `cd_livro_receita` int(10) unsigned NOT NULL,
  `ds_tagname` varchar(255) default NULL,
  PRIMARY KEY  (`id_tag_receita`,`cd_livro_receita`),
  KEY `tbl_tags_receita_FKIndex1` (`cd_livro_receita`),
  CONSTRAINT `tbl_tags_receita_ibfk_1` FOREIGN KEY (`cd_livro_receita`) REFERENCES `tbl_livros_receitas` (`id_livro_receita`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`tbl_tags_receita`
--

/*!40000 ALTER TABLE `tbl_tags_receita` DISABLE KEYS */;
INSERT INTO `tbl_tags_receita` (`id_tag_receita`,`cd_livro_receita`,`ds_tagname`) VALUES 
 (2,1,'tag2_l1'),
 (3,1,'tag3_l1'),
 (7,3,'tag1_l3'),
 (8,3,'tag2_l3'),
 (9,3,'tag3_l3');
/*!40000 ALTER TABLE `tbl_tags_receita` ENABLE KEYS */;


--
-- Table structure for table `test`.`usuario`
--

DROP TABLE IF EXISTS `usuario`;
CREATE TABLE `usuario` (
  `id_usuario` int(10) unsigned NOT NULL auto_increment,
  `ds_login` varchar(30) NOT NULL,
  `ds_password` varchar(20) NOT NULL,
  `ds_FullName` varchar(150) default NULL,
  `op_Blocked` tinyint(1) default NULL,
  PRIMARY KEY  (`id_usuario`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `test`.`usuario`
--

/*!40000 ALTER TABLE `usuario` DISABLE KEYS */;
INSERT INTO `usuario` (`id_usuario`,`ds_login`,`ds_password`,`ds_FullName`,`op_Blocked`) VALUES 
 (1,'fabio','123','f',0),
 (2,'admin','123','0',0),
 (3,'op','321','0',0),
 (4,'user','1','0',0);
/*!40000 ALTER TABLE `usuario` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
