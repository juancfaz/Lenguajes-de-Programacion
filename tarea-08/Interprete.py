archivo_entrada = open('program.txt', 'r')
texto_entrada = archivo_entrada.read()
lineas_programa = texto_entrada.split('\n')

vinculaciones = {}

def spaceless(str):
    return str.replace(' ', '')

def no_tiene_tercera_columna(tokens):
    return tokens[0] != None and tokens[1] != None and tokens[2] == None

def no_tiene_primera_columna(tokens):
    return tokens[0] == None and tokens[1] != None and tokens[2] != None

def es_pregunta(tokens):
    return tokens[1][-1] == '?'

def es_linea_entrada(tokens):
    return no_tiene_tercera_columna(tokens) and es_pregunta(tokens)

def es_linea_asignacion(tokens):
    return no_tiene_tercera_columna(tokens) and not es_pregunta(tokens)

def es_linea_salida(tokens):
    return no_tiene_primera_columna(tokens) and not es_pregunta(tokens)

def execute_entradas(var_nombre, pregunta):
    var_nombre = spaceless(var_nombre)
    vinculaciones[var_nombre] = int(input(pregunta + ' '))

def execute_asignacion(var_nombre, operacion):
    var_nombre = spaceless(var_nombre)
    operacion = spaceless(operacion)
    declaracion = var_nombre + '=' + operacion
    exec(declaracion, vinculaciones)

def execute_salida(operacion, formato_salida):
    execute_asignacion('__tmpvar__', operacion)
    print(formato_salida.replace('__', str(vinculaciones['__tmpvar__'])))

def execute(ast):
    for linea in ast:
        line_type = linea[0]
        params = linea[1:]
        
        if(line_type == 'ENTRADA'): execute_entradas(*params)
        if(line_type == 'ASIGNACION'): execute_asignacion(*params)
        if(line_type == 'SALIDA'): execute_salida(*params)

def tokenize(linea):
    tokens = linea.split('|')
    tokens = [t.strip() for t in tokens]
    tokens = [None if t=='' else t for t in tokens]
    
    if(len(tokens) == 2):
        tokens.append(None)
    if(len(tokens) != 3):
        raise Exception('Demasiadas columnas - {}', linea)

    return tokens

def parse(tokens, linea):
    if(es_linea_entrada(tokens)):
        return ('ENTRADA', tokens[0], tokens[1])
    elif(es_linea_asignacion(tokens)):
        return ('ASIGNACION', tokens[0], tokens[1])
    elif(es_linea_salida(tokens)):
        return ('SALIDA', tokens[1], tokens[2])
    else:
        raise Exception('Tipo de linea no soportada - {}', linea)

ast = []

for linea in lineas_programa:
    if('|' in linea):
        tokens = tokenize(linea)
        linea_ast = parse(tokens, linea)
        ast.append(linea_ast)

execute(ast)