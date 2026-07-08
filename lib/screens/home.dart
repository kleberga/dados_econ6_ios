import 'package:archive/archive.dart';
import 'package:dados_economicos6/model/model.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:url_launcher/url_launcher.dart';
import 'TelaDados.dart';
import 'package:cloud_firestore/cloud_firestore.dart';
import 'package:flutter/services.dart' as rootBundle;
import 'dart:convert';
import 'package:diacritic/diacritic.dart';
import 'package:http/http.dart' as http;
import 'dart:io' show Platform;
import 'package:package_info_plus/package_info_plus.dart';


class Home extends StatefulWidget {
  const Home({super.key});
  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  bool _isLoading = false;
  List<Widget> buttons = [];

Future<void> checkForUpdate(BuildContext context) async {
  try {
      final packageInfo = await PackageInfo.fromPlatform();
      final currentVersion = packageInfo.version;
      final response = await http.get(
        Uri.parse("https://itunes.apple.com/lookup?id=6742714405&country=br"),
      );
      
      String? latestVersion;
      
    if (response.statusCode == 200) {
      final data = jsonDecode(response.body);
      // Garantimos que a resposta da Apple App Store tem dados antes de ler
      if (data['results'] != null && data['results'].isNotEmpty) {
        latestVersion = data['results'][0]['version'];
      }
    }

      if (latestVersion != null && latestVersion != currentVersion) {
        if (!mounted) return;
        final shouldUpdate = await showDialog<bool>(
          context: context,
          builder: (_) => AlertDialog(
            title: const Text('Atualização disponível'),
            content: const Text('Uma nova versão está disponível na App Store. Deseja atualizar agora?'),
            actions: [
              TextButton(onPressed: () => Navigator.pop(context, false), child: const Text('Mais tarde')),
              ElevatedButton(onPressed: () => Navigator.pop(context, true), child: const Text('Atualizar')),
            ],
          ),
        );
        if (shouldUpdate == true) {
          final url = Uri.parse("https://apps.apple.com/br/app/dadoseco/id6742714405");
          if (await canLaunchUrl(url)) {
            await launchUrl(url, mode: LaunchMode.externalApplication);
          }
        }
      }
  } catch (e) {
    debugPrint('Falha ao verificar atualização: $e');
  }
}

  void fetchData() async {
    FirebaseFirestore firestore = FirebaseFirestore.instance;
    // carregar o arquivo GZIP
    ByteData data = await rootBundle.rootBundle.load('lib/database/assunto.json.gz');
    // Decodificar o arquivo Gzip
    List<int> bytes = data.buffer.asUint8List();
    List<int> decompressed = GZipDecoder().decodeBytes(bytes);
    // Decode os dados em JSON
    String jsonString = utf8.decode(decompressed);
    List<dynamic> jsonData = json.decode(jsonString);
    // transformar os dados
    List<Assunto> transformedData = jsonData.map((item) => Assunto.fromJson(item)).toList();

    transformedData.sort((a, b) => removeDiacritics(a.nome).compareTo(removeDiacritics(b.nome)));
    //List<Widget> fetchedButtons = snapshot.docs.map((doc) {

    List<Widget> fetchedButtons = transformedData.map((doc) {
      return Padding(
        padding: EdgeInsets.only(top: 20, bottom: 10),
        child: Center(
          child:
          Container(
            height: 40,
            width: 250,
            decoration: BoxDecoration(
                color: Colors.grey[200],
                borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                  BoxShadow(
                      color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                      blurRadius: 5) //blur radius of shadow
                ]
            ),
            child: ElevatedButton(
                style: ButtonStyle(
                  fixedSize: WidgetStateProperty.all(Size(210, 40)),
                  backgroundColor: WidgetStateProperty.all(corFundo,),
                  overlayColor: WidgetStateProperty.resolveWith<Color?>((states) {
                    if (states.contains(WidgetState.pressed)) {
                      return Colors.blue[500]; // Background color when clicked
                    }
                    return null; // Keep default overlay for other states
                  }),
                ),
                onPressed: () async {
                  setState(() {
                    _isLoading = true;
                  });
                  Navigator.push(
                    context,
                    MaterialPageRoute(
                        builder: (context) => TelaDados(assuntoSerie: doc.normalized_nome,)
                    ),
                  ).then((_) {
                    setState(() {
                      _isLoading = false;
                    });
                  });
                },
                child: Text(doc.nome, style: TextStyle(fontSize: 16, color: Colors.white),)
            ),
          ),
        ),
      );
    }).toList();
    setState(() {
      buttons = fetchedButtons;
    });
  }

  Future<void> _launchUrl() async {
    final Uri emailUri = Uri(
      scheme: 'mailto',
      path: 'dados.economicos156@gmail.com',
      query: 'subject=Contato',
    );
    if (!await launchUrl(emailUri)) {
      throw Exception('Could not launch $emailUri');
    }
  }

  @override
  void initState() {
    super.initState();
      WidgetsBinding.instance.addPostFrameCallback((_) {
        checkForUpdate(context);
    });
    fetchData();
  }

  var corFundo = Color.fromARGB(255, 63, 81, 181);
    final List<Widget> _circular = [
      Padding(padding: EdgeInsets.only(top: 20)),
      Center(child: Text("Carregando...", style: TextStyle(fontSize: 22, fontWeight: FontWeight.bold), ),)
    ];


  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text("Escolha o assunto", style: TextStyle(color: Colors.white)),
        backgroundColor: corFundo,
        iconTheme: IconThemeData(
          color: Colors.white, //change your color here
        ),
        leading: Builder(
          builder: (context) {
            return IconButton(
              icon: const Icon(Icons.menu),
              onPressed: () {
                Scaffold.of(context).openDrawer();
              },
            );
          },
        ),
      ),
      body:
      SingleChildScrollView(
        child: Stack(
          children: [
            Padding(
              padding: const EdgeInsets.only(bottom: 70),
              child: Column(
                children: _isLoading ? _circular : buttons,
              ),
            ),
          ],
        ),
      ),
      drawer: Drawer(
        child: ListView(
          padding: EdgeInsets.zero,
          children: [
            const DrawerHeader(
              decoration: BoxDecoration(
                color: Color.fromARGB(255, 63, 81, 181)
              ),
              child: Text("Menu", style: TextStyle(fontSize: 22, color: Colors.white)),
            ),
            ListTile(
              leading: const Icon(Icons.home),
              title: const Text("Home"),
              onTap: (){
                Navigator.pop(context);
              },
            ),
            ListTile(
              leading: const Icon(Icons.help),
              title: const Text("Ajuda e feedback"),
              onTap: (){
                _launchUrl();
              },
            )
          ],
        ),
      ),
    );
  }
}
