*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTPPE_BDE_001_1.................................*
DATA:  BEGIN OF STATUS_ZTPPE_BDE_001_1               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPPE_BDE_001_1               .
CONTROLS: TCTRL_ZTPPE_BDE_001_1
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTPPE_BDE_001_1               .
TABLES: ZTPPE_BDE_001_1                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
