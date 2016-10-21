#!/bin/bash
export LANG=C
export LC_MESSAGES=C
svn info ./zeoslib/packages       | grep "Revision" | awk '{print "zeosPackageRev   = \x27 ZeosLib package Revision: " $2 "\x27" ";" }'        >  src/revision.inc
svn info ./zeoslib/src            | grep "Revision" | awk '{print "zeosSourcesRev   = \x27 ZeosLib sources Revision: " $2 "\x27" ";" }'        >> src/revision.inc
svn info ./zeoslib/lib/postgresql | grep "Revision" | awk '{print "zeosPQLibRev     = \x27 ZeosLib PostgreSQL Lib Revision: " $2 "\x27" ";" }' >> src/revision.inc
svn info ./zeoslib/lib/mysql      | grep "Revision" | awk '{print "zeosMySQLLibRev  = \x27 ZeosLib MySQL Library Revision: " $2 "\x27" ";" }'  >> src/revision.inc
svn info ./zeoslib/lib/sqlite     | grep "Revision" | awk '{print "zeosSQLiteLibRev = \x27 ZeosLib SQLite Library Revision: " $2 "\x27" ";" }' >> src/revision.inc
svn info ./zeoslib/test           | grep "Revision" | awk '{print "zeosTestsRev     = \x27 ZeosLib Tests Revision: " $2 "\x27" ";" }'          >> src/revision.inc
svn info ./zeoslib/database       | grep "Revision" | awk '{print "zeosDBaseRev     = \x27 ZeosLib databases Revision: " $2 "\x27" ";" }'      >> src/revision.inc
svn info ./bgrabitmap             | grep "Revision" | awk '{print "bgrabitmapRev    = \x27 BGRABitmap Revision: " $2 "\x27" ";" }'             >> src/revision.inc
svn info ./bgracontrols           | grep "Revision" | awk '{print "bgracontrolsRev  = \x27 BGRAControls Revision: " $2 "\x27" ";" }'           >> src/revision.inc
svn info ./src/common             | grep "Revision" | awk '{print "pSCADACommonRev  = \x27 PascalSCADA Common Revision: " $2 "\x27" ";" }'     >> src/revision.inc
svn info ./src/database           | grep "Revision" | awk '{print "pSCADADBaseRev   = \x27 PascalSCADA Database Revision: " $2 "\x27" ";" }'   >> src/revision.inc
svn info ./src/hmi                | grep "Revision" | awk '{print "pSCADAHClRev     = \x27 PascalSCADA HCl Revision: " $2 "\x27" ";" }'        >> src/revision.inc
svn info ./src/scada              | grep "Revision" | awk '{print "pSCADAScadaRev   = \x27 PascalSCADA SCADA Revision: " $2 "\x27" ";" }'      >> src/revision.inc
