// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'lista_meus_dados.dart';

// **************************************************************************
// RiverpodGenerator
// **************************************************************************

// GENERATED CODE - DO NOT MODIFY BY HAND
// ignore_for_file: type=lint, type=warning

@ProviderFor(ListaMeusDados)
const listaMeusDadosProvider = ListaMeusDadosProvider._();

final class ListaMeusDadosProvider
    extends $NotifierProvider<ListaMeusDados, List<DadosSeries>> {
  const ListaMeusDadosProvider._()
      : super(
          from: null,
          argument: null,
          retry: null,
          name: r'listaMeusDadosProvider',
          isAutoDispose: true,
          dependencies: null,
          $allTransitiveDependencies: null,
        );

  @override
  String debugGetCreateSourceHash() => _$listaMeusDadosHash();

  @$internal
  @override
  ListaMeusDados create() => ListaMeusDados();

  /// {@macro riverpod.override_with_value}
  Override overrideWithValue(List<DadosSeries> value) {
    return $ProviderOverride(
      origin: this,
      providerOverride: $SyncValueProvider<List<DadosSeries>>(value),
    );
  }
}

String _$listaMeusDadosHash() => r'ed3aad531d8baccfbfca35b729cc5d4f6430a57e';

abstract class _$ListaMeusDados extends $Notifier<List<DadosSeries>> {
  List<DadosSeries> build();
  @$mustCallSuper
  @override
  void runBuild() {
    final created = build();
    final ref = this.ref as $Ref<List<DadosSeries>, List<DadosSeries>>;
    final element = ref.element as $ClassProviderElement<
        AnyNotifier<List<DadosSeries>, List<DadosSeries>>,
        List<DadosSeries>,
        Object?,
        Object?>;
    element.handleValue(ref, created);
  }
}
