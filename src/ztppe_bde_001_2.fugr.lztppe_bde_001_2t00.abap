*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTPPE_BDE_001_2.................................*
DATA:  BEGIN OF STATUS_ZTPPE_BDE_001_2               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPPE_BDE_001_2               .
CONTROLS: TCTRL_ZTPPE_BDE_001_2
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTPPE_BDE_001_2               .
TABLES: ZTPPE_BDE_001_2                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
