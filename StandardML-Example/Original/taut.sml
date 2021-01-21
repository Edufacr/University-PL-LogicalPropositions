(* Determinar si una proposición es una tautología *)

exception NoEsUnaTautologia of (Identificador * bool) list

fun taut prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val bools = gen_bools n
    	(* generar evaluaciones de la proposición*)
    	fun eval booleanos =
    		let
	    		fun recorrer []              = true
	    		|   recorrer (fila :: mas_filas) = 
	        		    let
	    		        	val asignacion = as_vals variables fila
	    		        	val evaluacion_es_verdadera = evalProp asignacion prop
	    		        in
	    		        	if evaluacion_es_verdadera then
	    		        		recorrer mas_filas
	    		        	else
	    		        	    raise NoEsUnaTautologia asignacion
	    		        end
            in
                recorrer bools
            end
    in
    	if eval bools then imprimir prop ^ " SÍ es una tautología" else ""
    end handle NoEsUnaTautologia asignacion => imprimir prop ^ " NO es una tautología, porque " ^ impr_as_vals asignacion
;
