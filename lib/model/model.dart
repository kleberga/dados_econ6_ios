import 'package:cloud_firestore/cloud_firestore.dart';

class DadosSeries {
  final String categoria;
  final String descricao;
  final String fonte;
  final String formato;
  final String idAssunto;
  final String localidades;
  final String metrica;
  final String nivelGeografico;
  final String nome;
  final String nomeCompleto;
  final String numero;
  final String periodicidade;
  final String urlAPI;

  DadosSeries({
    required this.categoria,
    required this.descricao,
    required this.fonte,
    required this.formato,
    required this.idAssunto,
    required this.localidades,
    required this.metrica,
    required this.nivelGeografico,
    required this.nome,
    required this.nomeCompleto,
    required this.numero,
    required this.periodicidade,
    required this.urlAPI,
  });

  factory DadosSeries.fromFirestore(DocumentSnapshot doc) {
    Map data = doc.data() as Map;
    return DadosSeries(
      categoria: data['categoria'] ?? '',
      descricao: data['descricao'] ?? '',
      fonte: data['fonte'] ?? '',
      formato: data['formato'] ?? '',
      idAssunto: data['idAssunto'] ?? '',
      localidades: data['localidades'] ?? '',
      metrica: data['metrica'] ?? '',
      nivelGeografico: data['nivelGeografico'] ?? '',
      nome: data['nome'] ?? '',
      nomeCompleto: data['nomeCompleto'] ?? '',
      numero: data['numero'] ?? '',
      periodicidade: data['periodicidade'] ?? '',
      urlAPI: data['urlAPI'] ?? ''
    );
  }
  factory DadosSeries.fromJson(Map<String, dynamic> data) {
    return DadosSeries(
        categoria: data['categoria'] ?? '',
        descricao: data['descricao'] ?? '',
        fonte: data['fonte'] ?? '',
        formato: data['formato'] ?? '',
        idAssunto: data['idAssunto'] ?? '',
        localidades: data['localidades'] ?? '',
        metrica: data['metrica'] ?? '',
        nivelGeografico: data['nivelGeografico'] ?? '',
        nome: data['nome'] ?? '',
        nomeCompleto: data['nomeCompleto'] ?? '',
        numero: data['numero'] ?? '',
        periodicidade: data['periodicidade'] ?? '',
        urlAPI: data['urlAPI'] ?? ''
    );
  }

  factory DadosSeries.fromJson_2(Map<String, dynamic> data) {
    return DadosSeries(
        categoria: data['classificacoes'] ?? '',
        descricao: data['descricao'] ?? '',
        fonte: data['fonte'] ?? '',
        formato: data['formato'] ?? '',
        idAssunto: data['idAssunto'] ?? '',
        localidades: data['localidades'] ?? '',
        metrica: data['metrica'] ?? '',
        nivelGeografico: data['nivelGeografico'] ?? '',
        nome: data['nome'] ?? '',
        nomeCompleto: data['nomeCompleto'] ?? '',
        numero: data['numero'] ?? '',
        periodicidade: data['periodicidade'] ?? '',
        urlAPI: data['urlAPI'] ?? ''
    );
  }

  // Adicione este método na sua classe:
  @override
  String toString() {
    return 'DadosSeries(nome: $nome)';
  }
}

class Assunto{
  final int id;
  final String nome;
  final String normalized_nome;

  Assunto({
    required this.id,
    required this.nome,
    required this.normalized_nome
  });

  factory Assunto.fromJson(Map<String, dynamic> json){
    return Assunto(id: json['id'], nome: json['nome'], normalized_nome: json['normalized_nome']);
  }
}