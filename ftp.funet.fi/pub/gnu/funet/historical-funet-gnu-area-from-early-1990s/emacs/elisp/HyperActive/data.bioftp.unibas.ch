               Setup of the Server bioftp.unibas.ch
               ====================================

                                    ~/
                 +-------------------+-----------------------------+
                 |                                                 |
               biology                                         programs 
   +-------------+-----------+--------------------+      uu and zoo source code
   |                         |                    |     
   EMBnet                    cd                  database
   |           The last EMBL CD-ROM (resources    +------------------------+
   |          permitting - drive used elsewehere)                          |
   |                                                                       |
   +-----+-------------+-----------+--------------------+                  |
         |             |           |                    |                  |
    XSwissprot         |     XEMBL.VAX_ZOO           Xembl                 |
(not yet; scheduled)   |           |                    |                  |
    +----+--+          |   zoo'ed    VAX/VMS       The contents            |
data.dir   indices.dir |   GCG / NBRF formatted    of the dir's            |
Updates to the current |   current XEMBL.ref       below containing        |
Swissprot database     |   and XEMBL.seq files     Entry code, and         |
                       |   as well as XXEMBL       title line              |
            DATABASE.ZOO                                 |                 |
The zoo'ed databases currently                           |                 |
availavle at BIOCENTER in VAX        +--------------------------+          |
/VMS and GCG/NBRF formatted          |                          |          | 
zoo files: EMBL,SWISSPROT,PIR        |                          |          |
                                   data.dir                    indices.dir |
+----+----+----+----+----+----+----+-+--+----+----+----+----+  containing  |
fun  inv  mam  org  phg  pln  pri  pro  rod  syn  una vrl vrt  indices in  |
Entries in EMBL format, *ONE PER FILE*. The files are named    EMBL format |
using the entry code as file name. NEW in the next release;                | 
extension 'dat', update to existing release, extension 'upd'.              |
                                                                           |
Variuos databases available on various other servers as well,mainly updated, 
                           but not entirely produced, by Amos Bairoch, Geneva 
                                                                           |
 +---+----+------+---+----+---+---+----+------+----+-----+--+---+----+----++
 |   |    |      |   |    |   |   |    |      |    |     |  |   |    |    |
ecd  | fans_ref  |  info  |  ngdd |  seqanalr |  enzyme  | fly  | jour_tab|
     |           |        |       |           |          |      |       trna
   prosite      tfd      alu      epd     gcg_codon    limb   rebase
