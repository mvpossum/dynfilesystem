
-record(fsstate, {id,%id del filesystem(=al del worker)
                  folder,%carpeta donde guardar los archivos locales
                  %files,%archivos (remotos y locales)
                  lockedfiles,%archivos lockeados por algun worker
                  openlocalfiles,
                  clientfiles
                  
                  
                  %openfiles,%archivos abiertos
                  %openlocalfiles,%archivos locales abiertos
                  %maybefiles,%archivos locales que quieren crearse
                  %clientfiles%archivos abiertos por clientes de este worker
                  }).

