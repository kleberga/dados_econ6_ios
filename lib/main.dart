import 'package:dados_economicos6/model/variables_class.dart';
import 'package:flutter/material.dart';
import 'package:google_mobile_ads/google_mobile_ads.dart';
import 'screens/home.dart';
import 'package:firebase_core/firebase_core.dart';
import 'infra/firebase_options.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_dotenv/flutter_dotenv.dart';
import 'package:flutter_localizations/flutter_localizations.dart';

List<cadastroSeries> listaIBGE = [];
List<Metrica> listaMetrica = [];
List<NivelGeografico> listaNivelGeografico = [];
List<Localidades> listaLocalidades = [];
List<Categorias> listaCategorias = [];
var listajaCombinada;

final providerContainer = ProviderContainer();

Future<void> main() async {

  // inicializar uma instancia de WidgetsFlutterBinding. In the Flutter framework,
  // the WidgetsFlutterBinding class plays a crucial role. It is responsible for
  // the application's lifecycle, handling input gestures, and triggering the build
  // and layout of widgets.
  WidgetsFlutterBinding.ensureInitialized();
  MobileAds.instance.initialize();
  await Firebase.initializeApp(
    options: DefaultFirebaseOptions.currentPlatform,
  );

  runApp(
      ProviderScope(
        //parent: providerContainer,
        overrides: [
          // Add any provider overrides here, if needed.
          // For example: myProvider.overrideWithValue(someValue),
        ],
        child: MaterialApp(
          locale: const Locale('pt', 'BR'),
          supportedLocales: const [
            Locale('pt', 'BR'), // Portuguese (Brazil)
        ],
          localizationsDelegates: [
            GlobalMaterialLocalizations.delegate,
            GlobalWidgetsLocalizations.delegate,
            GlobalCupertinoLocalizations.delegate, // Add this for Cupertino widgets
          ],
        home: Home(),
        debugShowCheckedModeBanner: false,
      ),
    )
);
}
