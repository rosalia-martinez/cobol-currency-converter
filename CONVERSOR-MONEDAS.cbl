      ******************************************************************
      * Author: ROSALIA MARTINEZ HERNANDEZ
      * Date: 04/01/2026
      * Tectonics: cobc
      * Description: CONVERT EUR TO USD AND GBP
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERSOR-MONEDAS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *-----------------------------*
      * VARIABLES
      *-----------------------------*
       01 WS-OPCION          PIC 9(1)    VALUE ZERO.
       01 WS-CANTIDAD        PIC 9(5)V99 VALUE ZEROS.
       01 WS-RESULTADO       PIC 9(5)V99 VALUE ZEROS.
       01 WS-TASA-USADA      PIC 9V99999 VALUE ZEROS.
       01 WS-MONEDA-ORIGEN   PIC X(3)    VALUE SPACES.
       01 WS-MONEDA-DESTINO  PIC X(3)    VALUE SPACES.

       01 WS-TASAS-CAMBIO.
           05 WS-EUR-USD     PIC 9V99999 VALUE 1.17.
           05 WS-USD-EUR     PIC 9V99999 VALUE 0.85.
           05 WS-EUR-GBP     PIC 9V99999 VALUE 0.87.
           05 WS-GBP-EUR     PIC 9V99999 VALUE 1.15.

       01 WS-PANTALLA.
           05 WS-CANTIDAD-DISPLAY  PIC ZZZZ9.99.
           05 WS-RESULTADO-DISPLAY PIC ZZZZ9.99.
      *----------------------------*
       PROCEDURE DIVISION.
      *----------------------------*
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-OPCION = 5
               PERFORM MOSTRAR-MENU
               PERFORM PROCESAR-OPCION
           END-PERFORM

           DISPLAY " "
           DISPLAY "GRACIAS POR USAR EL CONVERSOR!"
           STOP RUN.
      *----------------------------*
       MOSTRAR-MENU.
           DISPLAY "========================================"
           DISPLAY "          CONVERSOR DE MONEDAS          "
           DISPLAY "========================================"
           DISPLAY "1. Euros a Dolares (EUR -> USD)"
           DISPLAY "2. Dolares a Euros (USD -> EUR)"
           DISPLAY "3. Euros a Libras (EUR -> GBP)"
           DISPLAY "4. Libras a Euros (GBP -> EUR)"
           DISPLAY "5. Salir"
           DISPLAY " "
           DISPLAY "Seleccione una opcion (1-5): ".
      *----------------------------*
       PROCESAR-OPCION.
           ACCEPT WS-OPCION

           EVALUATE TRUE
               WHEN WS-OPCION = 5
                   CONTINUE
               WHEN WS-OPCION < 1 OR WS-OPCION > 5
                   DISPLAY " "
                   DISPLAY "ERROR: Opcion " WS-OPCION " no valida"
                   DISPLAY "Por favor, elija del 1 al 5"
               WHEN OTHER
                   PERFORM PEDIR-CANTIDAD
                   PERFORM REALIZAR-CONVERSION
                   PERFORM MOSTRAR-RESULTADO
           END-EVALUATE.
      *----------------------------*
       PEDIR-CANTIDAD.
           DISPLAY " "
           DISPLAY "Introduce la cantidad a convertir: "
           ACCEPT WS-CANTIDAD-DISPLAY
           MOVE WS-CANTIDAD-DISPLAY TO WS-CANTIDAD.

      *----------------------------*
       REALIZAR-CONVERSION.
           EVALUATE WS-OPCION
               WHEN 1
                   MOVE "EUR" TO WS-MONEDA-ORIGEN
                   MOVE "USD" TO WS-MONEDA-DESTINO
                   MOVE WS-EUR-USD TO WS-TASA-USADA
                   COMPUTE WS-RESULTADO = WS-CANTIDAD * WS-EUR-USD
               WHEN 2
                   MOVE "USD" TO WS-MONEDA-ORIGEN
                   MOVE "EUR" TO WS-MONEDA-DESTINO
                   MOVE WS-USD-EUR TO WS-TASA-USADA
                   COMPUTE WS-RESULTADO = WS-CANTIDAD * WS-USD-EUR
               WHEN 3
                   MOVE "EUR" TO WS-MONEDA-ORIGEN
                   MOVE "GBP" TO WS-MONEDA-DESTINO
                   MOVE WS-EUR-GBP TO WS-TASA-USADA
                   COMPUTE WS-RESULTADO = WS-CANTIDAD * WS-EUR-GBP
               WHEN 4
                   MOVE "GBP" TO WS-MONEDA-ORIGEN
                   MOVE "EUR" TO WS-MONEDA-DESTINO
                   MOVE WS-GBP-EUR TO WS-TASA-USADA
                   COMPUTE WS-RESULTADO = WS-CANTIDAD * WS-GBP-EUR
           END-EVALUATE.

      *-----------------------------*
       MOSTRAR-RESULTADO.
           MOVE WS-CANTIDAD TO WS-CANTIDAD-DISPLAY
           MOVE WS-RESULTADO TO WS-RESULTADO-DISPLAY

           DISPLAY " "
           DISPLAY "=============== RESULTADO ==============="
           DISPLAY "Moneda origen:  " WS-MONEDA-ORIGEN
           DISPLAY "Moneda destino: " WS-MONEDA-DESTINO
           DISPLAY "Tasa de cambio: " WS-TASA-USADA
           DISPLAY "-----------------------------------------"
           DISPLAY "Cantidad original:  " WS-CANTIDAD-DISPLAY
           " " WS-MONEDA-ORIGEN
           DISPLAY "Cantidad convertida: " WS-RESULTADO-DISPLAY
           " " WS-MONEDA-DESTINO
           DISPLAY "========================================="
           DISPLAY " ".

       END PROGRAM CONVERSOR-MONEDAS.
