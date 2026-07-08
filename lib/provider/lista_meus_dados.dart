// import 'package:flutter/foundation.dart';
// import '../model/model.dart';
//
// class ListaMeusDados extends ChangeNotifier {
//   List<DadosSeries> _listaEscolhida = [];
//
//   List<DadosSeries> get getListaEscolhida => _listaEscolhida;
//
//   void setListaEscolhida(List<DadosSeries> value) {
//     _listaEscolhida = value;
//     notifyListeners();
//   }
// }

import 'package:riverpod_annotation/riverpod_annotation.dart';
import '../model/model.dart';

// The part directive tells the compiler that the generated file
// should be considered part of this file.
part 'lista_meus_dados.g.dart';

@riverpod
class ListaMeusDados extends _$ListaMeusDados {
  // `build` is where you set the initial state.
  @override
  List<DadosSeries> build() {
    return [];
  }

  // The method to update the state.
  // We use `state = ...` to automatically notify listeners.
  void setListaEscolhida(List<DadosSeries> value) {
    state = value;
  }
}