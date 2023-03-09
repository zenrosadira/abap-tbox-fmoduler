## ABAP Dynamic Function Module
A technical tool designed to easily manage dynamic ABAP Function Module calls.

# Usage
Create new object of class ZTBOX_CL_FMODULER passing function module name.
```
DATA(function) = NEW ztbox_cl_fmoduler( 'BAPI_BATCH_CREATE' ).
```

Pass parameters using EXPORTING, CHANGING and IMPORTING methods.
Use CHANGING method for "TABLES" parameters too.
IMPORTING method can be called also after the function execution if the parameter is not generically typed.
```
function->exporting( i_name = 'MATERIAL' i_value = '123' ).
function->exporting( i_name = 'PLANT' i_value = '001' ).
function->exporting( i_name = 'BATCHATTRIBUTES' i_value = VALUE bapibatchatt( expirydate = sy-datum + 365 ) ).

DATA t_return TYPE bapiret2_t.
function->changing( EXPORTING i_name = 'RETURN' CHANGING c_value = t_return ).
```

Run function by calling EXECUTE method.
```
function->execute( ).

DATA batch_created TYPE charg_d.
function->importing( EXPORTING i_name = 'BATCH' CHANGING c_value = batch_created ).

LOOP AT t_return INTO DATA(ret).
 WRITE: ret-message, /.
ENDLOOP.
```

Use EXCEPTION method to access the possible exception raised by the function
```
DATA(exception) = function->exception( ).
WRITE: |Exception code: { exception-subrc } - Exception name: { exception-except } - Exception message: { exception-message }|.
```

## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)
