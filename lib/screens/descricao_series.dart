import 'package:dados_economicos6/screens/TelaDados.dart';
import 'package:flutter/material.dart';

var corFundo = Color.fromARGB(255, 63, 81, 181);

extension StringExtension on String {
  String capitalize() {
    return "${this[0].toLowerCase()}${this.substring(1)}";
  }
}

extension StringExtension2 on String {
  String capitalize2() {
    return "${this[0].toUpperCase()}${this.substring(1).toLowerCase()}";
  }
}

class DescricaoSeries extends StatelessWidget {

  final String cod_series;
  const DescricaoSeries({Key? key, required this.cod_series}) : super(key: key);
  const DescricaoSeries.otherConstructor(this.cod_series);

  @override
  Widget build(BuildContext context) {

    return Scaffold(
        appBar: AppBar(
          title: Text("Descrição da série", style: TextStyle(color: Colors.white),),
          iconTheme: IconThemeData(
            color: Colors.white, //change your color here
          ),
          backgroundColor: corFundo,
        ),
        body: ListView.builder(
          itemCount: 1,
          itemBuilder: (context, index) {
            return ListTile(
              title: Text(listaEscolhida.firstWhere((element) => element.numero==cod_series ).nomeCompleto,
                style: TextStyle(fontWeight: FontWeight.bold),),
              subtitle: RichText(
                text: TextSpan(
                    children: <TextSpan>[
                      TextSpan(text: '\nDescrição: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).descricao), // Remaining text
                      TextSpan(text: '\n'), // Remaining text
                      TextSpan(text: '\nNível geográfico: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).nivelGeografico), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nLocalidade: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).localidades), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nGrupo: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).categoria), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nForma de cálculo: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).metrica), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nFormato da série: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).formato), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nPeriodicidade de divulgação: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).periodicidade), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nPeríodo disponível: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: 'entre $dataInicialSerie e $dataFinalSerie'), // Remaining text
                      TextSpan(text: '\n'),
                      TextSpan(text: '\nFonte dos dados: ', style: TextStyle(fontWeight: FontWeight.bold)),
                      TextSpan(text: listaEscolhida.firstWhere((element) => element.numero==cod_series).fonte), // Remaining text
                      TextSpan(text: '\n'),
                    ], style: DefaultTextStyle.of(context).style, ),
                textAlign: TextAlign.justify,
              ),
            );
          },
        )
    );
  }
}