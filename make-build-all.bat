@REM Windows batch file script that replicates the function of 'make build-all'.

@ECHO OFF

SETLOCAL
FOR /F "delims=" %%F IN ('stack path --local-bin') DO (
SET STACKLOCALBINDIR=%%F
)

REM build-all
CALL :build
CALL :build-docs
EXIT /B

:build
CALL :hie-8.2.1
CALL :hie-8.2.2
CALL :hie-8.4.2
CALL :hie-8.4.3
CALL :hie-8.4.4
EXIT /B

:hie-8.2.1
CALL :submodules
stack --stack-yaml=stack-8.2.1.yaml install happy
stack --stack-yaml=stack-8.2.1.yaml install
COPY "%STACKLOCALBINDIR%\hie.exe" "%STACKLOCALBINDIR%\hie-8.2.1.exe"
COPY "%STACKLOCALBINDIR%\hie-8.2.1.exe" "%STACKLOCALBINDIR%\hie-8.2.exe"
EXIT /B

:hie-8.2.2
CALL :submodules
stack --stack-yaml=stack-8.2.2.yaml install happy
stack --stack-yaml=stack-8.2.2.yaml install
COPY "%STACKLOCALBINDIR%\hie.exe" "%STACKLOCALBINDIR%%\hie-8.2.2.exe"
COPY "%STACKLOCALBINDIR%\hie-8.2.2.exe" "%STACKLOCALBINDIR%\hie-8.2.exe"
EXIT /B

:hie-8.4.2
CALL :submodules
stack --stack-yaml=stack-8.4.2.yaml install
COPY "%STACKLOCALBINDIR%\hie.exe" "%STACKLOCALBINDIR%\hie-8.4.2.exe"
COPY "%STACKLOCALBINDIR%\hie-8.4.2.exe" "%STACKLOCALBINDIR%\hie-8.4.exe"
EXIT /B

:hie-8.4.3
CALL :submodules
stack --stack-yaml=stack-8.4.3.yaml install
COPY "%STACKLOCALBINDIR%\hie.exe" "%STACKLOCALBINDIR%\hie-8.4.3.exe"
COPY "%STACKLOCALBINDIR%\hie-8.4.3.exe" "%STACKLOCALBINDIR%\hie-8.4.exe"
EXIT /B

:hie-8.4.4
CALL :submodules
stack --stack-yaml=stack-8.4.4.yaml install
COPY "%STACKLOCALBINDIR%\hie.exe" "%STACKLOCALBINDIR%\hie-8.4.4.exe"
COPY "%STACKLOCALBINDIR%\hie-8.4.4.exe" "%STACKLOCALBINDIR%\hie-8.4.exe"
EXIT /B

:submodules
git submodule update --init
EXIT /B

:build-docs
stack --stack-yaml=stack-8.2.1.yaml exec hoogle generate
stack --stack-yaml=stack-8.2.2.yaml exec hoogle generate
stack --stack-yaml=stack-8.4.2.yaml exec hoogle generate
stack --stack-yaml=stack-8.4.3.yaml exec hoogle generate
stack --stack-yaml=stack-8.4.4.yaml exec hoogle generate
EXIT /B
