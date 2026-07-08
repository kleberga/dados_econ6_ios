import 'package:intl/intl.dart';
import '../screens/TelaDados.dart';

class serie_app {
  final DateTime data;
  final double valor;
  serie_app(this.data, this.valor);
  factory serie_app.fromJson(Map<String, dynamic> parsedJson){
    var w;
    var data2;
    w = double.parse(parsedJson['valor']);
    if(formatoData=='dd/MM/yyyy'){
      data2 = DateFormat('dd/MM/yyyy').parse(parsedJson['data']);
    } else {
      data2 = DateFormat('MM/yyyy').parse(parsedJson['data'].substring(3));
    }
    return serie_app(data2, w);
  }
  @override
  toString(){
    return "data: $data, valor: $valor";
  }
  List<dynamic> toRow(){
    return [data, valor];
  }
}

class serie_app_focus {
  final DateTime DataReferencia;
  final double Mediana;
  serie_app_focus(this.DataReferencia, this.Mediana);
  factory serie_app_focus.fromJson(Map<String, dynamic> parsedJson){
    var w;
    var data2;
    if(formatoData=='dd/MM/yyyy'){
      data2 = DateFormat('dd/MM/yyyy').parse(parsedJson['DataReferencia']);
    } else if(formatoData == "MM/yyyy"){
      data2 = DateFormat('MM/yyyy').parse(parsedJson['DataReferencia']);
    } else {
      data2 = DateFormat('yyyy').parse(parsedJson['DataReferencia']);
    }
    return serie_app_focus(data2, parsedJson['Mediana']);
  }
  @override
  toString(){
    return "data: $DataReferencia, valor: $Mediana";
  }
}

class cadastroSeries {
  final String numero;
  final String nome;
  final String nomeCompleto;
  final String descricao;
  final String formato;
  final String fonte;
  final String urlAPI;
  final int idAssunto;
  final String periodicidade;
  final String metrica;
  final String nivelGeografico;
  final String localidades;
  final String categoria;
  cadastroSeries(
      this.numero, this.nome, this.nomeCompleto, this.descricao, this.formato,
      this.fonte, this.urlAPI, this.idAssunto, this.periodicidade, this.metrica,
      this.nivelGeografico, this.localidades, this.categoria);
  @override
  toString(){
    return "numero $numero, nome: $nome, nomeCompleto: $nomeCompleto, descricao: $descricao, formato: $formato, fonte: $fonte, urlAPI: $urlAPI, idAssunto: $idAssunto, periodicidade: $periodicidade, metrica: $metrica, nivelGeografico: $nivelGeografico, localidade: $localidades, categoria: $categoria";
  }
}

var numeroIndicePrecos = 0;

class Metrica{
  int id;
  String nome;
  Metrica({required this.id, required this.nome});
  @override
  toString(){
    return "id: $id nome: $nome";
  }
}

class NivelGeografico{
  String id;
  String nome;
  NivelGeografico({required this.id, required this.nome});
  @override
  toString(){
    return "id: $id, nome: $nome";
  }
}

class Localidades{
  int id;
  String nome;
  String nivelGeografico;
  Localidades({required this.id, required this.nome, required this.nivelGeografico});
  @override
  toString(){
    return "id: $id, nome: $nome, nivelGeografico: $nivelGeografico";
  }
}

class Categorias{
  int id;
  String nome;
  Categorias({required this.id, required this.nome});
  @override
  toString(){
    return "id: $id, nome: $nome";
  }
}