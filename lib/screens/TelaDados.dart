import 'dart:convert';
import 'dart:core';
import 'dart:io';
import 'package:archive/archive.dart';
import 'package:dados_economicos6/screens/reportar_erro.dart';
import 'package:dados_economicos6/model/variables_class.dart';
import 'package:flutter/cupertino.dart';
import 'package:flutter/services.dart';
import 'package:google_mobile_ads/google_mobile_ads.dart';
import 'package:http/http.dart' as http;
import 'package:flutter/material.dart';
import 'package:intl/intl.dart' hide TextDirection;
import 'package:month_picker_dialog/month_picker_dialog.dart';
import 'package:path_provider/path_provider.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:share_plus/share_plus.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:syncfusion_flutter_charts/charts.dart';
import '../infra/database_helper.dart';
import 'descricao_series.dart';
import '../model/model.dart';
import 'package:cloud_firestore/cloud_firestore.dart';
import '../main.dart';
import 'package:diacritic/diacritic.dart';
import 'package:flutter/services.dart' as rootBundle;
import 'package:dados_economicos6/provider/lista_meus_dados.dart';


String dropdownValue = "";
String dropdownValueMetrica = "";
String dropdownValueNivelGeog = "";
String dropdownValueLocalidade = "";
String dropdownValueCategoria = "";
String urlSerie = '';
var service;
var ultimaDataIPCA;
String? meuNumeroTeste;
List<String> listaMostrar = [];
List<String> listaMostrarMetrica = [];
List<String> listaMostrarNivelGeog = [];
List<String> listaMostrarLocalidade = [];
List<String> listaMostrarCategoria = [];
List<String>  listaMostrarCategoria_aux = [];

var fonte;
var cod_serie;
//var initialIndex;
List<Toggle_reg>? valorToggle = [];
var isNotificationGranted;
var nomeSerie;
var formatoSerie;
late var codAssunto;
var dataInicialSerie;
var dataFinalSerie;
var metricaValue;
var alturaCategoria;
var valorItemHeightCategoria;
var alturaSerie;
var valorItemHeightSerie;
var periodicidade;
var alturaMetrica;
var valorItemHeightMetrica;
var formatoData;
var formatoDataGrafico;
var f = NumberFormat('#,##0.00', 'pt_BR');
var listaAnosSerieAnual = [];
var anoInicialSelecionado;
var anoFinalSelecionado;
var dtInicial;
var notFound = false;
var notFoundText;
var api1Future;
var jsonString2;

DateTime startval1 = DateFormat('MM/yyyy').parse('01/2021');
DateTime endval1 = DateFormat('MM/yyyy').parse('12/2021');
var corFundo = Color.fromARGB(255, 63, 81, 181);
List<DadosSeries> listaEscolhida = [];

class TelaDados extends StatefulWidget {
  final String assuntoSerie;
  const TelaDados({required this.assuntoSerie,});
  @override
  State<TelaDados> createState() => _TelaDados();
}

Future<String> getStringFromLocalStorage(String key) async {
  final prefs = await SharedPreferences.getInstance();
  return prefs.getString(key) ?? '';
}

class _TelaDados extends State<TelaDados> {


  InterstitialAd? _interstitialAd;
  bool isAdLoaded = false;
  bool hasAdBeenShown = false;
  final adUnitId = Platform.isAndroid
      ? 'ca-app-pub-5086029981237773/2909242477'
     //  ? 'ca-app-pub-3940256099942544/1033173712'
    //  : 'ca-app-pub-5086029981237773~1901760712';
        : 'ca-app-pub-5086029981237773/9616933315';

  void loadAd() {
    InterstitialAd.load(
        adUnitId: adUnitId,
        request: const AdRequest(),
        adLoadCallback: InterstitialAdLoadCallback(
          // Called when an ad is successfully received.
          onAdLoaded: (ad) {
            ad.fullScreenContentCallback = FullScreenContentCallback(
              // Called when the ad showed the full screen content.
                onAdShowedFullScreenContent: (ad) {},
                // Called when an impression occurs on the ad.
                onAdImpression: (ad) {},
                // Called when the ad failed to show full screen content.
                onAdFailedToShowFullScreenContent: (ad, err) {
                  // Dispose the ad here to free resources.
                  ad.dispose();
                },
                // Called when the ad dismissed full screen content.
                onAdDismissedFullScreenContent: (ad) {
                  // Dispose the ad here to free resources.
                  ad.dispose();

                  setState(() {
                    _interstitialAd = null;
                    isAdLoaded = false;
                  });
                },
                // Called when a click is recorded for an ad.
                onAdClicked: (ad) {});
            debugPrint('$ad loaded.');
            // Keep a reference to the ad so you can show it later.
            _interstitialAd = ad;
            isAdLoaded = true;
            if(!hasAdBeenShown) {
              //_showInterstitialAd();
              Future.delayed(Duration(seconds: 10), () {
                _showInterstitialAd();
              });
            }
          },
          // Called when an ad request failed.
          onAdFailedToLoad: (LoadAdError error) {
            debugPrint('InterstitialAd failed to load: $error');
          },
        )
    );
  }
  void _showInterstitialAd() {
    if (isAdLoaded && _interstitialAd != null) {
      _interstitialAd!.fullScreenContentCallback = FullScreenContentCallback(
        onAdDismissedFullScreenContent: (ad) {
          ad.dispose();
          _interstitialAd = null; // Clean up the ad after it's shown
        },
        onAdFailedToShowFullScreenContent: (ad, error) {
          ad.dispose();
          _interstitialAd = null; // Clean up the ad in case of an error
        },
      );
      _interstitialAd!.show();
      hasAdBeenShown = true; // Mark the flag to prevent showing again
    }
  }

  Future<http.Response> getJsonFromRestAPI(String url_serie) async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.setString('nomeSerieArmaz', nomeSerie);
    await prefs.setString('urlSerieArmaz', urlSerie);
    await prefs.setString('fonteSerieArmaz', fonte);
    await prefs.setString("codigoArmaz", cod_serie);
    await prefs.setString("metricaArmaz", dropdownValueMetrica);
    await prefs.setString("localidadeArmaz", dropdownValueLocalidade);
    await prefs.setString("categoriaArmaz", dropdownValueCategoria);
    String url = url_serie;
    http.Response response = await http.get(Uri.parse(url));
    return response;
  }

  List<serie_app> chartData = [];

  TextEditingController dateInputEnd = TextEditingController();
  TextEditingController dateInputIni = TextEditingController();

  Future loadDataSGS() async {
    setState(() {
      listaAnosSerieAnual.clear();
      notFound = false;
    });
    http.Response response;
    String jsonString;
    var contador = 0;
    var urlAjustada;
    if(periodicidade=="diária"){
      DateTime today = DateTime.now();
      DateTime originalDate = DateTime(today.year, today.month, today.day);
      DateTime dateTenYearsBefore = DateTime(
        originalDate.year - 10,
        originalDate.month,
        originalDate.day,
      );
      urlAjustada = urlSerie + '&dataInicial=' + dateTenYearsBefore.day.toString().padLeft(2,'0') + '/' + dateTenYearsBefore.month.toString().padLeft(2,'0') + '/' + dateTenYearsBefore.year.toString();
    } else {
      urlAjustada = urlSerie;
    }
    do {
      response = await getJsonFromRestAPI(urlAjustada);
      jsonString = response.body;
      contador = contador + 1;
    } while(response.statusCode!=200 && contador<=10);
    if(response.statusCode!=200){
      setState(() {
        notFound = true;
        notFoundText = "Dados não disponíveis no momento. A fonte dos dados desta série pode estar temporariamente indisponível. Tente mais tarde!";
      });
    } else {
      setState(() {
        notFound = false;
      });
      jsonString2 = jsonString.replaceAll('<?xml version="1.0" encoding="pt-br"?>', '');
      jsonString2 = jsonString2.replaceAll('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"', '');
      jsonString2 = jsonString2.replaceAll('"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">', '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"');
      jsonString2 = jsonString2.replaceAll('<html xmlns="http://www.w3.org/1999/xhtml" lang="pt-br" xml:lang="pt-br">', '');
      var pattern11 = RegExp(r'<head>\s*{[^}]*}');
      jsonString2 = jsonString2.replaceAll(pattern11, '');
      jsonString2 = jsonString2.replaceAll('<head>', '');
      jsonString2 = jsonString2.replaceAll('<title>Requisição inválida!</title>', '');
      jsonString2 = jsonString2.replaceAll('<link rev="made" />', '');
      jsonString2 = jsonString2.replaceAll('<style>', '');
      var pattern1 = RegExp(r'a:link\s*{[^}]*}');
      jsonString2 = jsonString2.replaceAll(pattern1, '');
      var pattern2 = RegExp(r'a:hover\s*{[^}]*}');
      jsonString2 = jsonString2.replaceAll(pattern2, '');
      var pattern4 = RegExp(r'a:active\s*{[^}]*}');
      jsonString2 = jsonString2.replaceAll(pattern4, '');
      var pattern5 = RegExp(r'a:visited\s*{[^}]*}');
      jsonString2 = jsonString2.replaceAll(pattern5, '');
      final jsonResponse = json.decode(jsonString2);
      if(chartData.isNotEmpty){
        chartData.clear();
      }
      setState(() {
        for (Map<String, dynamic> i in jsonResponse){
          if(i['valor']!=""&&i['valor']!=null){
            chartData.add(serie_app.fromJson(i));
          }
        }
        chartData.sort((a, b){ //sorting in descending order
          return a.data.compareTo(b.data);
        });
        endval1 = chartData.last.data;
        dataInicialSerie = DateFormat(formatoData).format(chartData.first.data).toString();
        dataFinalSerie = DateFormat(formatoData).format(chartData.last.data).toString();
        listaAnosSerieAnual = chartData.map((e) => e.data.toString().substring(0,4)).toSet().toList();
        if(chartData.length>13){
          startval1 = chartData[chartData.length-13].data;
        } else {
          startval1 = chartData.first.data;
        }
        if(listaAnosSerieAnual.length>13){
          anoInicialSelecionado = listaAnosSerieAnual.length-13;
        } else {
          anoInicialSelecionado = listaAnosSerieAnual.length;
        }
        if(listaAnosSerieAnual.length>0){
          anoFinalSelecionado = listaAnosSerieAnual.length-1;
        } else {
          anoFinalSelecionado = listaAnosSerieAnual.length;
        }
      });
      ultimaDataIPCA = chartData.last.data;
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('dataFinal', endval1.toString());
    }
  }

  Future loadDataFocus() async {
    setState(() {
      listaAnosSerieAnual.clear();
      notFound = false;
    });
    http.Response response;
    String jsonString;
    var contador = 0;
    do {
      response = await getJsonFromRestAPI(urlSerie);
      jsonString = response.body;
      contador = contador + 1;
    } while(response.statusCode!=200 && contador<=10);
    if(response.statusCode!=200){
      setState(() {
        notFound = true;
        notFoundText = "Dados não disponíveis no momento. A fonte dos dados desta série pode estar temporariamente indisponível. Tente mais tarde!";
      });
    } else {
      setState(() {
        notFound = false;
      });
      jsonString2 = jsonString.replaceAll('<?xml version="1.0" encoding="pt-br"?>', '');
      jsonString2 = jsonString2.replaceAll('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"', '');
      jsonString2 = jsonString2.replaceAll('"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">', '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"');
      jsonString2 = jsonString2.replaceAll('<html xmlns="http://www.w3.org/1999/xhtml" lang="pt-br" xml:lang="pt-br">', '');
      jsonString2 = jsonString2.replaceAll('<head>', '');
      jsonString2 = jsonString2.replaceAll('<title>Requisição inválida!</title>', '');
      jsonString2 = jsonString2.replaceAll('<link rev="made" />', '');
      jsonString2 = jsonString2.replaceAll('<style>', '');
      jsonString2 = jsonString2.replaceAll('a:visited {color:#AF8C3A; text-decoration: none;}', '');
      final jsonResponse = json.decode(jsonString2);
      if(chartData.isNotEmpty){
        chartData.clear();
      }
      List<serie_app_focus> chartDataFocus = [];
      for (Map<String, dynamic> i in jsonResponse['value']){
        if(i['Mediana']!=""&&i['Mediana']!=null){
          if(periodicidade=="trimestral") {
            var w = i['DataReferencia'].toString().substring(0,1);
            if(w=="1"){
              w = "03";
            } else if(w=="2"){
              w = "06";
            } else if(w=="3"){
              w = "09";
            } else {
              w = "12";
            }
            var x = w + "/" + i['DataReferencia'].toString().substring(2, 6);
            chartDataFocus.add(serie_app_focus(DateFormat('MM/yyyy').parse(x), i['Mediana']));
          } else {
            chartDataFocus.add(serie_app_focus.fromJson(i));
          }
        }
      }
      setState(() {
        for (int i = 0; i < chartDataFocus.length; i++){
          if(chartDataFocus[i].Mediana!=""){
            chartData.add(serie_app(chartDataFocus[i].DataReferencia, chartDataFocus[i].Mediana));
          }
        }
        chartData.sort((a, b){ //sorting in descending order
          return a.data.compareTo(b.data);
        });
        endval1 = chartData[chartData.length-1].data;
        startval1 = chartData[0].data;
        dataInicialSerie = DateFormat(formatoData).format(chartData.first.data).toString();
        dataFinalSerie = DateFormat(formatoData).format(chartData.last.data).toString();
        listaAnosSerieAnual = chartData.map((e) => e.data.toString().substring(0,4)).toSet().toList();
        anoInicialSelecionado = 0;
        anoFinalSelecionado = listaAnosSerieAnual.length-1;
      });
      ultimaDataIPCA = chartData.last.data;
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('dataFinal', endval1.toString());
    }
  }
  NumberFormat formatter1 = new NumberFormat("00");
  NumberFormat formatter2 = new NumberFormat("0000");

  Future loadDataIBGE() async {
    setState(() {
      listaAnosSerieAnual.clear();
      notFound = false;
    });
    http.Response response;
    String jsonString;
    var contador = 0;
    do {
      response = await getJsonFromRestAPI(urlSerie);
      jsonString = response.body;
      contador = contador + 1;
    } while(response.statusCode!=200 && contador<=10);
    if(response.statusCode!=200){
      setState(() {
        notFound = true;
        notFoundText = "Dados não disponíveis no momento. A fonte dos dados desta série pode estar temporariamente indisponível. Tente mais tarde!";
      });
    } else {
      setState(() {
        notFound = false;
      });
      var pattern1 = RegExp(r'/* #4D749F */\s*{[^}]*}');
      jsonString2 = jsonString.replaceAll(pattern1, '');
      jsonString2 = jsonString2.replaceAll('/* #4D749F */', '');
      final jsonResponse = json.decode(jsonString2);
      final item = jsonResponse[0]['resultados'][0]['series'][0]['serie'];
      if(chartData.isNotEmpty){
        chartData.clear();
      }
      setState(() {
        for (var i = 0; i<item.keys.toList().length; i++){
          var x = item.keys.toList()[i];
          var w = formatter1.format(int.parse(x.substring(4)));
          if(periodicidade=="trimestral") {
            if(w=="01"){
              w = "03";
            } else if(w=="02"){
              w = "06";
            } else if(w=="03"){
              w = "09";
            } else {
              w = "12";
            }
          }
          if(periodicidade=="semestral") {
            if(w=="01"){
              w = "06";
            } else {
              w = "12";
            }
          }
          x = w + "/" + formatter2.format(int.parse(x.substring(0, 4)));
          var y = item.values.toList()[i].toString();
          if(y!="..."&&y!="-"&&y!="X"&&y!=".."){
            chartData.add(
                serie_app(
                    DateFormat('MM/yyyy').parse(x),
                    double.parse(y)
                )
            );
          }
        }
        chartData.sort((a, b){ //sorting in descending order
          return a.data.compareTo(b.data);
        });
        if(chartData.isEmpty){
          setState(() {
            notFound = true;
            notFoundText = "Esta série não possui valores! Altere os filtros da pesquisa.";
          });
        } else {
          setState(() {
            notFound = false;
          });
          dataInicialSerie = DateFormat(formatoData).format(chartData.first.data).toString();
          dataFinalSerie = DateFormat(formatoData).format(chartData.last.data).toString();
          ultimaDataIPCA = chartData.last.data;
          listaAnosSerieAnual = chartData.map((e) => e.data.toString().substring(0,4)).toSet().toList();
          if(listaAnosSerieAnual.length>13){
            anoInicialSelecionado = listaAnosSerieAnual.length-13;
          } else {
            anoInicialSelecionado = listaAnosSerieAnual.length;
          }
          if(listaAnosSerieAnual.length>0){
            anoFinalSelecionado = listaAnosSerieAnual.length-1;
          } else {
            anoFinalSelecionado = listaAnosSerieAnual.length;
          }
          endval1 = chartData.last.data;
          if(chartData.length>13){
            startval1 = chartData[chartData.length-13].data;
          } else {
            startval1 = chartData.first.data;
          }
        }
      });
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('dataFinal', endval1.toString());
    }
  }

  Future loadDataIPEADATA() async {
    setState(() {
      listaAnosSerieAnual.clear();
      notFound = false;
    });
    http.Response response;
    String jsonString;
    var contador = 0;
    do {
      response = await getJsonFromRestAPI(urlSerie);
      jsonString = response.body;
      contador = contador + 1;
    } while(response.statusCode!=200 && contador<=10);
    if(response.statusCode!=200){
      setState(() {
        notFound = true;
        notFoundText = "Dados não disponíveis no momento. A fonte dos dados desta série pode estar temporariamente indisponível. Tente mais tarde!";
      });
    } else {
      setState(() {
        notFound = false;
      });
      var pattern1 = RegExp(r'/* #4D749F */\s*{[^}]*}');
      jsonString2 = jsonString.replaceAll(pattern1, '');
      jsonString2 = jsonString2.replaceAll('/* #4D749F */', '');
      final jsonResponse = json.decode(jsonString2);
      final item = jsonResponse['value'];
      if(chartData.isNotEmpty){
        chartData.clear();
      }
      setState(() {
        for (var i = 0; i<item.length; i++){
          var x = item[i]['VALDATA'];
          var w = formatter1.format(int.parse(x.substring(5,7)));
          var ano = formatter2.format(int.parse(x.substring(0, 4)));
          if(periodicidade=="trimestral") {
            if(w=="01"){
              w = "03";
            } else if(w=="04"){
              w = "06";
            } else if(w=="07"){
              w = "09";
            } else {
              w = "12";
            }
            x = w + "/" + ano;
          }
          if(periodicidade=="semestral") {
            if(w=="01"){
              w = "06";
            } else {
              w = "12";
            }
            x = w + "/" + ano;
          }
          if(periodicidade=="diária") {
            var dia = formatter2.format(int.parse(x.substring(8, 10)));
            x = dia + "/" + w + "/" + ano;
          }
          if(periodicidade == "anual"){
            x = ano;
          }
          if(periodicidade=="mensal") {
            x = w + "/" + ano;
          }
          var y = item[i]['VALVALOR'].toString();
          if(y!="..."&&y!="-"&&y!="X"&&y!="null"){
            chartData.add(
                serie_app(
                    DateFormat(formatoData).parse(x),
                    double.parse(y)
                )
            );
          }
        }
        chartData.sort((a, b){ //sorting in descending order
          return a.data.compareTo(b.data);
        });
        if(chartData.isEmpty){
          setState(() {
            notFound = true;
            notFoundText = "Esta série não possui valores! Altere os filtros da pesquisa.";
          });
        } else {
          setState(() {
            notFound = false;
          });
          dataInicialSerie = DateFormat(formatoData).format(chartData.first.data).toString();
          dataFinalSerie = DateFormat(formatoData).format(chartData.last.data).toString();
          ultimaDataIPCA = chartData.last.data;
          listaAnosSerieAnual = chartData.map((e) => e.data.toString().substring(0,4)).toSet().toList();
          if(listaAnosSerieAnual.length>13){
            anoInicialSelecionado = listaAnosSerieAnual.length-13;
          } else {
            anoInicialSelecionado = listaAnosSerieAnual.length;
          }
          if(listaAnosSerieAnual.length>0){
            anoFinalSelecionado = listaAnosSerieAnual.length-1;
          } else {
            anoFinalSelecionado = listaAnosSerieAnual.length;
          }
          endval1 = chartData.last.data;
          if(chartData.length>13){
            startval1 = chartData[chartData.length-13].data;
          } else {
            startval1 = chartData.first.data;
          }
        }
      });
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('dataFinal', endval1.toString());
    }
  }

  Future loadDataRTN() async {
    setState(() {
      listaAnosSerieAnual.clear();
      notFound = false;
    });
    http.Response response;
    String jsonString;
    var contador = 0;
    do {
      response = await getJsonFromRestAPI(urlSerie);
      jsonString = response.body;
      contador = contador + 1;
    } while(response.statusCode!=200 && contador<=10);
    if(response.statusCode!=200){
      setState(() {
        notFound = true;
        notFoundText = "Dados não disponíveis no momento. A fonte dos dados desta série pode estar temporariamente indisponível. Tente mais tarde!";
      });
    } else {
      setState(() {
        notFound = false;
      });
      var pattern1 = RegExp(r'/* #4D749F */\s*{[^}]*}');
      jsonString2 = jsonString.replaceAll(pattern1, '');
      jsonString2 = jsonString2.replaceAll('/* #4D749F */', '');
      final jsonResponse = json.decode(jsonString2);
      final item = jsonResponse['registros'];

      if(chartData.isNotEmpty){
        chartData.clear();
      }
      setState(() {
        for (var i = 0; i<item.length; i++){
          var x = item[i]['data'];
          var w = formatter1.format(int.parse(x.substring(5,7)));
          var ano = formatter2.format(int.parse(x.substring(0, 4)));
          x = w + "/" + ano;
          var y = item[i]['valor'].toString();
          if(y!="..."&&y!="-"&&y!="X"&&y!="null"){
            chartData.add(
                serie_app(
                    DateFormat(formatoData).parse(x),
                    double.parse(y)
                )
            );
          }
        }
        chartData.sort((a, b){ //sorting in descending order
          return a.data.compareTo(b.data);
        });
        if(chartData.isEmpty){
          setState(() {
            notFound = true;
            notFoundText = "Esta série não possui valores! Altere os filtros da pesquisa.";
          });
        } else {
          setState(() {
            notFound = false;
          });
          dataInicialSerie = DateFormat(formatoData).format(chartData.first.data).toString();
          dataFinalSerie = DateFormat(formatoData).format(chartData.last.data).toString();
          ultimaDataIPCA = chartData.last.data;
          listaAnosSerieAnual = chartData.map((e) => e.data.toString().substring(0,4)).toSet().toList();
          if(listaAnosSerieAnual.length>13){
            anoInicialSelecionado = listaAnosSerieAnual.length-13;
          } else {
            anoInicialSelecionado = listaAnosSerieAnual.length;
          }
          if(listaAnosSerieAnual.length>0){
            anoFinalSelecionado = listaAnosSerieAnual.length-1;
          } else {
            anoFinalSelecionado = listaAnosSerieAnual.length;
          }
          endval1 = chartData.last.data;
          if(chartData.length>13){
            startval1 = chartData[chartData.length-13].data;
          } else {
            startval1 = chartData.first.data;
          }
        }
      });
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('dataFinal', endval1.toString());
    }
  }

  List<serie_app> itemsBetweenDates({
    required List<serie_app> lista,
    required DateTime start,
    required DateTime end,
  }) {
    var output = <serie_app>[];
    for (var i = 0; i < lista.length; i += 1) {
      DateTime date = lista[i].data;
      if (date.compareTo(start) >= 0 && date.compareTo(end) <= 0) {
        output.add(lista[i]);
      }
    }
    return output;
  }

  FirebaseFirestore firestore = FirebaseFirestore.instance;

  Future<List<DadosSeries>> fetchDados_lspa(String url_meta, String url_loc) async {
    final response = await http.get(Uri.parse(url_meta));
    final response2 = await http.get(Uri.parse(url_loc));
    if (response.statusCode == 200 && response2.statusCode == 200) {
      // 1. Decodifica o JSON (retorna uma List<dynamic>)
      Map<String, dynamic> jsonMap = jsonDecode(response.body);

      List<dynamic> categorias1 = jsonMap['classificacoes'][0]['categorias'];
      List<dynamic> categorias2 = jsonMap['classificacoes'][1]['categorias'];
      List<dynamic> variaveis = jsonMap['variaveis'];
      List<dynamic> nivel_geog = jsonDecode(response2.body);
      List<Map<String,dynamic>> nivel_geog2 = nivel_geog.map((dados) {
        return{
          'id': dados['id'],
          'nome': dados['nome'],
          'nivel_id': dados['nivel']['id'],
          'nivel_nome': dados['nivel']['nome']
        };
      }).toList().toSet().toList();

      List<Map<String, dynamic>> lista_aux = [];
      var formato_unidade;
      for(var i in categorias1){
        for(var w in categorias2){
          for(var j in nivel_geog2){
            for(var k in variaveis){
            if(k["nome"] == "Área plantada" || k["nome"] == "Área colhida"){
                formato_unidade = "Hectares";
            } else {
                formato_unidade =  w['unidade'];
            }
              lista_aux.add({
                'numero': "${i['nome']}-${w['nome']}-${j['nivel_id']}-${j['id']}",
                'nome': jsonMap['pesquisa'],
                'nomeCompleto': jsonMap['pesquisa'],
                'descricao': "O Levantamento Sistemático da Produção Agrícola tem por objetivo fornecer informações estatísticas sobre o plantio, colheita, produção e rendimento médio, de forma sistemática, para os principais produtos das lavouras permanentes e temporárias. É uma pesquisa de previsão e acompanhamento das variáveis área, produção e rendimento médio de 25 importantes produtos agrícolas, desde a fase de intenção de plantio até o final da colheita, de cada cultura investigada dentro do ano civil corrente e prognóstico da safra subsequente.",
                'formato': formato_unidade,
                'fonte': 'IBGE',
                'urlAPI': 'https://servicodados.ibge.gov.br/api/v3/agregados/${jsonMap['id']}/periodos/all/variaveis/${k['id']}?localidades=${j['nivel_id']}[${j['id']}]&classificacao=49[${i['id']}]|48[${w['id']}]',
                'idAssunto': "9",
                'periodicidade': jsonMap['periodicidade']['frequencia'],
                'metrica': k['nome'],
                'nivelGeografico': j['nivel_nome'],
                'localidades': j['nome'],
                'categoria': '${i['nome']} - ${w['nome']}'
              });
            }
          }
        }
      }

      // 2. Converte para uma lista de objetos da sua classe
      return lista_aux.map((item) => DadosSeries.fromJson(item)).toList();
    } else {
      throw Exception('Falha ao carregar dados');
    }
  }


  Future<List<DadosSeries>> fetchData(String codAssunto) async {
    var arquivo = 'lib/database/'+codAssunto+'.json.gz';
    // carregar o arquivo GZIP
    ByteData data = await rootBundle.rootBundle.load(arquivo);
    // Decodificar o arquivo Gzip
    List<int> bytes = data.buffer.asUint8List();
    List<int> decompressed = GZipDecoder().decodeBytes(bytes);
    // Decode os dados em JSON
    String jsonString = utf8.decode(decompressed);
    List<dynamic> jsonData = json.decode(jsonString);

    // transformar os dados
    List<DadosSeries> transformedData = jsonData.map((item) => DadosSeries.fromJson(item)).toList();

    var enderecos_lspa = [
      {"url_meta": 'https://servicodados.ibge.gov.br/api/v3/agregados/188/metadados',
      'url_loc': 'https://servicodados.ibge.gov.br/api/v3/agregados/188/localidades/N1%7CN2%7CN3'},
      {"url_meta": 'https://servicodados.ibge.gov.br/api/v3/agregados/1618/metadados',
          'url_loc': 'https://servicodados.ibge.gov.br/api/v3/agregados/1618/localidades/N1%7CN2%7CN3'}
    ];

    if(codAssunto == "agropecuaria"){
      var tarefas = enderecos_lspa.map((endereco) {
        return fetchDados_lspa(endereco['url_meta']!, endereco['url_loc']!);
      });

      // 2. Executa todas em paralelo e espera o resultado de todas
      List<List<DadosSeries>> resultadosApi = await Future.wait(tarefas);

      // Achata a lista de listas em uma lista única
      List<DadosSeries> listaApiPlana = resultadosApi.expand((lista) => lista).toList();

      // 3. Achata a lista de listas em uma lista única
      transformedData = [...transformedData, ...listaApiPlana];
    }

    // ordenar os dados
    transformedData.sort((a, b) => removeDiacritics(a.nome).compareTo(removeDiacritics(b.nome)));
    return transformedData;
  }

  Future<void> showPermissionDialog(BuildContext context) async {
    showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text('Permissão requerida'),
          content: Text(
            'Este aplicativo requer permissão para salvar e compartilhar arquivos. Por favor, habilite a permissão.',
          ),
          actions: [
            TextButton(
              onPressed: () {
                Navigator.of(context).pop();
              },
              child: Text('Cancelar'),
            ),
            TextButton(
              onPressed: () async {
                Navigator.of(context).pop();
                await openAppSettings(); // Opens app settings for the user to enable permissions
              },
              child: Text('Configurações'),
            ),
          ],
        );
      },
    );
  }

  Future<void> createAndShareCSV() async {

    PermissionStatus status = await Permission.manageExternalStorage.status;

      final DateFormat formatter = DateFormat('yyyy-MM-dd');
      // Generate CSV data
      String csvData = 'data,valor\n';

      for (var item in chartData) {
        String formattedDate = formatter.format(item.data);
        csvData += '$formattedDate,${item.valor}\n';
      }
      // Save the CSV file to the Downloads folder
      String filePath;
      if(Platform.isAndroid){
        Directory downloadsDir = await getTemporaryDirectory();
        filePath = '${downloadsDir.path}/series.csv';
      } else {
        final directory = await getApplicationDocumentsDirectory();
        filePath = '${directory.path}/series.csv';
      }
      File file = File(filePath);
      await file.writeAsString(csvData);
      // Create a XFile instance
      XFile csvFile = XFile(filePath, mimeType: 'text/csv');

      // Share the CSV file
      Share.shareXFiles([csvFile], text: 'Here’s the CSV file!');
  }
//late Future<List<DadosSeries>> api1Future;

  @override
  void initState() {
    // TODO: implement initState
    super.initState();
    loadAd();
    codAssunto = widget.assuntoSerie;
    api1Future = fetchData(codAssunto);

    api1Future.then((data) {
        setState(() {

          var listaMeusDados = providerContainer.read(listaMeusDadosProvider.notifier);

          listaMeusDados.setListaEscolhida(data);
          listaEscolhida = providerContainer.read(listaMeusDadosProvider);

          listaMostrar = listaEscolhida.map((element) => element.nome.toString()).toList().toSet().toList();

          listaMostrar.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
          dropdownValue = listaMostrar.isNotEmpty ? listaMostrar.first : '';

          listaMostrarMetrica = listaEscolhida.where((element) => element.nome==dropdownValue).map((e) => e.metrica.toString()).toSet().toList();
          listaMostrarMetrica.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
          dropdownValueMetrica = listaMostrarMetrica.first;

          listaMostrarNivelGeog = listaEscolhida.where((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica).map((e) => e.nivelGeografico.toString()).toSet().toList();
          listaMostrarNivelGeog.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
          dropdownValueNivelGeog = listaMostrarNivelGeog.first;

          listaMostrarLocalidade = listaEscolhida.where((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog).map((e) => e.localidades.toString()).toSet().toList();
          listaMostrarLocalidade.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
          dropdownValueLocalidade = listaMostrarLocalidade.first;

          listaMostrarCategoria_aux = listaEscolhida.where((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade).map((e) => e.categoria.toString()).toSet().toList();

          // Separar o indice geral em um objeto a parte
          String? specialAssunto;
          List<String> otherAssuntos = [];
          for (var assunto in listaMostrarCategoria_aux) {
            if (removeDiacritics(assunto).toLowerCase() == 'indice geral') {
              specialAssunto = assunto;
            } else {
              otherAssuntos.add(assunto);
            }
          }
          // ordenar os dados
          otherAssuntos.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
          listaMostrarCategoria = [];
          // combinar o objeto com o indice geral e a lista ordenada
          if (specialAssunto != null) {
            listaMostrarCategoria.add(specialAssunto);
          }
          listaMostrarCategoria.addAll(otherAssuntos);

          dropdownValueCategoria = listaMostrarCategoria.first;

          urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;
          fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
          cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

          nomeSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).nome;

          formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;

          periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;

          if(periodicidade=="anual"){
            formatoData = "yyyy";
            formatoDataGrafico = "yyyy";
          } else if(periodicidade=="diária"){
            formatoData = "dd/MM/yyyy";
            formatoDataGrafico = "dd/MM/yy";
          } else {
            formatoData = "MM/yyyy";
            formatoDataGrafico = "MM/yy";
          }
          if(fonte == "Banco Central do Brasil"){
            loadDataSGS();
          } else if(fonte == "Banco Central do Brasil - Pesquisa Focus"){
            loadDataFocus();
          } else if(fonte == "IBGE") {
            loadDataIBGE();
          } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
            loadDataRTN();
          } else {
            loadDataIPEADATA();
          }
        });

    }).catchError((error) {
      setState(() {
        listaEscolhida = [];
      });
    });
    dropdownValue = "1";
  }

  void _showDialog(Widget child) async {
    showCupertinoModalPopup<void>(
      context: context,
      builder: (BuildContext context) => Container(
        height: 216,
        padding: const EdgeInsets.only(top: 6.0),
        // The Bottom margin is provided to align the popup above the system navigation bar.
        margin: EdgeInsets.only(
          bottom: MediaQuery.of(context).viewInsets.bottom,
        ),
        // Provide a background color for the popup.
        color: CupertinoColors.systemBackground.resolveFrom(context),
        // Use a SafeArea widget to avoid system overlaps.
        child: SafeArea(
          top: false,
          child: child,
        ),
      ),
    );
  }

  Widget metodoSelecaoInicial(String periodicidadeSerie){
    if(listaAnosSerieAnual.isNotEmpty){
      if (periodicidadeSerie == "mensal" || periodicidadeSerie == "trimestral" || periodicidadeSerie == "semestral") {
        return Column(
          children: [
            TextField(
              controller: dateInputIni,
              //editing controller of this TextField
              decoration: InputDecoration(
                  icon: Icon(Icons.calendar_today), //icon of text field
                  labelText: "Data Inicial:" //label text of field
              ),
              readOnly: true,
              //set it true, so that user will not able to edit text
              onTap: () async {
                Intl.defaultLocale = 'pt_BR';
                DateTime? pickedDate = await showMonthPicker(
                    context: context,
                    initialDate: startval1,
                    firstDate: DateFormat(formatoData).parse(dataInicialSerie),
                    lastDate: DateFormat(formatoData).parse(dataFinalSerie),
                  monthPickerDialogSettings: const MonthPickerDialogSettings(
                    headerSettings: PickerHeaderSettings(
                      headerCurrentPageTextStyle: TextStyle(fontSize: 14, color: Colors.white),
                      headerSelectedIntervalTextStyle: TextStyle(fontSize: 16, color: Colors.white),
                      headerBackgroundColor: Color.fromARGB(255, 63, 81, 181),
                    ),
                    dialogSettings: PickerDialogSettings(
                      dialogRoundedCornersRadius: 20,
                    ),
                      actionBarSettings: PickerActionBarSettings(
                          confirmWidget:  Text(
                            'OK',
                            style: TextStyle(
                              fontWeight: FontWeight.bold,
                              color: Color.fromARGB(255, 63, 81, 181),
                            ),
                          ),
                          cancelWidget: Text(
                            'Cancelar',
                            style: TextStyle(
                              fontWeight: FontWeight.bold,
                              color: Color.fromARGB(255, 63, 81, 181),
                            ),
                          )
                      )
                  ),
                );
                if (pickedDate != null) {
                  String formattedDate =
                  DateFormat(formatoData).format(pickedDate);
                  setState(() {
                    dateInputIni.text = formattedDate;
                    startval1 = DateFormat(formatoData).parse(formattedDate);
                  });
                } else {}
              },
            ),
            TextField(
              controller: dateInputEnd,
              //editing controller of this TextField
              decoration: InputDecoration(
                  icon: Icon(Icons.calendar_today), //icon of text field
                  labelText: "Data Final:" //label text of field
              ),
              readOnly: true,
              //set it true, so that user will not able to edit text
              onTap: () async {
                Intl.defaultLocale = 'pt_BR';
                DateTime? pickedDate = await showMonthPicker(
                    context: context,
                    initialDate: endval1,
                    firstDate: DateFormat(formatoData).parse(dataInicialSerie),
                    lastDate: DateFormat(formatoData).parse(dataFinalSerie),
                    monthPickerDialogSettings: const MonthPickerDialogSettings(
                      headerSettings: PickerHeaderSettings(
                        headerCurrentPageTextStyle: TextStyle(fontSize: 14, color: Colors.white),
                        headerSelectedIntervalTextStyle: TextStyle(fontSize: 16, color: Colors.white),
                        headerBackgroundColor: Color.fromARGB(255, 63, 81, 181),
                      ),
                      dialogSettings: PickerDialogSettings(
                        dialogRoundedCornersRadius: 20,
                      ),
                        actionBarSettings: PickerActionBarSettings(
                            confirmWidget:  Text(
                              'OK',
                              style: TextStyle(
                                fontWeight: FontWeight.bold,
                                color: Color.fromARGB(255, 63, 81, 181),
                              ),
                            ),
                            cancelWidget: Text(
                              'Cancelar',
                              style: TextStyle(
                                fontWeight: FontWeight.bold,
                                color: Color.fromARGB(255, 63, 81, 181),
                              ),
                            )
                        )
                    ),
                );
                if (pickedDate != null) {
                  String formattedDate =
                  DateFormat(formatoData).format(pickedDate);
                  setState(() {
                    dateInputEnd.text = formattedDate;
                    endval1 = DateFormat(formatoData).parse(formattedDate);
                  });
                } else {}
              },
            ),
          ],
        );
      } else if (periodicidadeSerie == "diária") {
          return Column(
            children: [
              TextField(
                controller: dateInputIni,
                //editing controller of this TextField
                decoration: InputDecoration(
                    icon: Icon(Icons.calendar_today), //icon of text field
                    labelText: "Data Inicial:" //label text of field
                ),
                readOnly: true,
                //set it true, so that user will not able to edit text
                onTap: () async {
                  Intl.defaultLocale = 'pt_BR';
                  DateTime? pickedDate = await showDatePicker(
                      context: context,
                      initialDate: startval1,
                      firstDate: DateFormat(formatoData).parse(dataInicialSerie),
                      lastDate: DateFormat(formatoData).parse(dataFinalSerie),
                    builder: (BuildContext context, Widget? child) {
                      return Theme(
                        data: ThemeData.light().copyWith(
                          primaryColor: Color.fromARGB(255, 63, 81, 181), // Header background color
                          colorScheme: ColorScheme.light(primary: Color.fromARGB(255, 63, 81, 181)),
                          buttonTheme: ButtonThemeData(textTheme: ButtonTextTheme.primary,),
                        ),
                        child: child!,
                      );
                    },

                  );
                  if (pickedDate != null) {
                    String formattedDate =
                    DateFormat(formatoData).format(pickedDate);
                    setState(() {
                      dateInputIni.text = formattedDate;
                      startval1 = DateFormat(formatoData).parse(formattedDate);
                    });
                  } else {}
                },
              ),
              TextField(
                controller: dateInputEnd,
                //editing controller of this TextField
                decoration: InputDecoration(
                    icon: Icon(Icons.calendar_today), //icon of text field
                    labelText: "Data Final:" //label text of field
                ),
                readOnly: true,
                //set it true, so that user will not able to edit text
                onTap: () async {
                  Intl.defaultLocale = 'pt_BR';
                  DateTime? pickedDate = await showDatePicker(
                      context: context,
                      initialDate: endval1,
                      firstDate: DateFormat(formatoData).parse(dataInicialSerie),
                      lastDate: DateFormat(formatoData).parse(dataFinalSerie),
                    builder: (BuildContext context, Widget? child) {
                      return Theme(
                        data: ThemeData.light().copyWith(
                          primaryColor: Color.fromARGB(255, 63, 81, 181), // Header background color
                          colorScheme: ColorScheme.light(primary: Color.fromARGB(255, 63, 81, 181)),
                          buttonTheme: ButtonThemeData(textTheme: ButtonTextTheme.primary),
                        ),
                        child: child!,
                      );
                    },
                  );
                  if (pickedDate != null) {
                    String formattedDate =
                    DateFormat(formatoData).format(pickedDate);
                    setState(() {
                      dateInputEnd.text = formattedDate;
                      endval1 = DateFormat(formatoData).parse(formattedDate);
                    });
                  } else {}
                },
              ),
            ],
          );
      } else {
        return Column(
          children: <Widget>[
            Text("Data inicial:"),
            CupertinoButton(
              padding: EdgeInsets.zero,
              // Display a CupertinoPicker with list of fruits.
              onPressed: () async => _showDialog(await
              CupertinoPicker(
                magnification: 1.22,
                squeeze: 1.2,
                useMagnifier: true,
                itemExtent: 32,
                // This sets the initial item.
                scrollController: FixedExtentScrollController(
                  initialItem: anoInicialSelecionado,
                ),
                // This is called when selected item is changed.
                onSelectedItemChanged: (int selectedItem) {
                  String formattedDate = DateFormat("MM/yyyy").parse("01/"+listaAnosSerieAnual[(selectedItem)]).toString();
                  setState(() {
                    anoInicialSelecionado = selectedItem;
                    dateInputIni.text = formattedDate;
                    startval1 = DateFormat("yyyy-MM-dd").parse(formattedDate);
                  });
                },
                children:
                List<Widget>.generate(listaAnosSerieAnual.length, (int index) {
                  return Center(child: Text(listaAnosSerieAnual[index]));
                }),
              ),
              ),
              // This displays the selected fruit name.
              child: Text(
                listaAnosSerieAnual[anoInicialSelecionado],
                style: const TextStyle(
                    fontSize: 18.0, color: Colors.black, decoration: TextDecoration.underline
                ),
              ),
            ),
            Text("Data final:"),
            CupertinoButton(
              padding: EdgeInsets.zero,
              // Display a CupertinoPicker with list of fruits.
              onPressed: () async => _showDialog(await
              CupertinoPicker(
                magnification: 1.22,
                squeeze: 1.2,
                useMagnifier: true,
                itemExtent: 32,
                // This sets the initial item.
                scrollController: FixedExtentScrollController(
                  initialItem: anoFinalSelecionado,
                ),
                // This is called when selected item is changed.
                onSelectedItemChanged: (int selectedItem) {
                  String formattedDate = DateFormat("MM/yyyy").parse("01/"+listaAnosSerieAnual[(selectedItem)]).toString();
                  setState(() {
                    anoFinalSelecionado = selectedItem;
                    dateInputEnd.text = formattedDate;
                    //startval1 = DateFormat("MM/yyyy").parse(formattedDate);
                    endval1 = DateFormat("yyyy-MM-dd").parse(formattedDate);
                  });
                },
                children:
                List<Widget>.generate(listaAnosSerieAnual.length, (int index) {
                  return Center(child: Text(listaAnosSerieAnual[index]));
                }),
              ),
              ),
              // This displays the selected fruit name.
              child: Text(
                listaAnosSerieAnual[anoFinalSelecionado],
                style: const TextStyle(
                    fontSize: 18.0, color: Colors.black, decoration: TextDecoration.underline
                ),
              ),
            ),
          ],
        );
      }
    } else {
      return CircularProgressIndicator();
    }
  }

  @override
  void dispose() {
    // TODO: Dispose an InterstitialAd object
    _interstitialAd?.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {

    filtrarDados(){
      dateInputIni.text = DateFormat(formatoData).format(startval1).toString();
      dateInputEnd.text = DateFormat(formatoData).format(endval1).toString();
      DateTime dataIni = DateFormat(formatoData).parse(dateInputIni.text.toString());
      DateTime dataFim = DateFormat(formatoData).parse(dateInputEnd.text.toString());
      late var lista_filtrada = itemsBetweenDates(lista: chartData, start: dataIni, end: dataFim);
      lista_filtrada = itemsBetweenDates(lista: chartData, start: dataIni, end: dataFim);
      lista_filtrada.sort((a, b){ //sorting in descending order
        return a.data.compareTo(b.data);
      });
      return lista_filtrada;
    }

    Size _textSize(String text, TextStyle style) {
      final TextPainter textPainter = TextPainter(
          text: TextSpan(text: text, style: style), maxLines: 1, textDirection: TextDirection.ltr)
        ..layout(minWidth: 0, maxWidth: double.infinity);
      return textPainter.size;
    }

    if(_textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=300){
      alturaCategoria = 40.0;
    } else if(_textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>300&&
        _textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=525){
      alturaCategoria = 60.0;
    } else if(_textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>525&&
        _textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=750){
      alturaCategoria = 80.0;
    } else {
      alturaCategoria = 110.0;
    }

    if(_textSize(dropdownValueCategoria, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>300) {
      valorItemHeightCategoria = 60.0;
    } else {
      valorItemHeightCategoria = 50.0;
    }

    if(_textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=327){
      alturaSerie = 40.0;
    } else if(_textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>327&&
        _textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=525){
      alturaSerie = 60.0;
    } else if(_textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>525&&
        _textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=750){
      alturaSerie = 80.0;
    } else {
      alturaSerie = 100.0;
    }

    if(_textSize(dropdownValue, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>300) {
      valorItemHeightSerie = 60.0;
    } else {
      valorItemHeightSerie = 50.0;
    }

    if(_textSize(dropdownValueMetrica, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=327){
      alturaMetrica = 40.0;
    } else if(_textSize(dropdownValueMetrica, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>327&&
        _textSize(dropdownValueMetrica, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width<=525){
      alturaMetrica = 60.0;
    } else  {
      alturaMetrica = 80.0;
    }
    if(_textSize(dropdownValueMetrica, TextStyle(fontWeight: FontWeight.normal, fontSize: 15)).width>300) {
      valorItemHeightMetrica = 60.0;
    } else {
      valorItemHeightMetrica = 50.0;
    }

    return Scaffold(
        appBar: AppBar(
          title: Text("Visualize os dados", style: TextStyle(color: Colors.white) ),
          backgroundColor: corFundo,
          iconTheme: IconThemeData(
            color: Colors.white, //change your color here
          ),
        ),
        body: SingleChildScrollView(
            child: Stack(
              children: <Widget>[
                Container(
                  padding: EdgeInsets.all(15),
                  child: Column(
                    children: <Widget>[
                      Text(
                        "Selecione a série:",
                        style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                      ),
                      Padding(padding: EdgeInsets.only(bottom: 5)),
                      Row(
                        children: <Widget>[
                          Expanded(
                                  child: Center(
                                    child: FutureBuilder<List<DadosSeries>>(
                                      future: api1Future,
                                      builder: (context, snapshot) {
                                      if (snapshot.connectionState == ConnectionState.waiting) {
                                      return Center(child: CircularProgressIndicator());
                                      } else if (snapshot.hasError) {
                                      return Center(child: Text('Error: ${snapshot.error}'));
                                      } else if (!snapshot.hasData || snapshot.data!.isEmpty) {
                                      return Center(child: Text('No data found.'));
                                      } else {
                                        List<DadosSeries> listaEscolhida = snapshot.data!;
                                        return  Column(
                                              children: <Widget>[
                                                Container(
                                                  height: alturaSerie,
                                                  width: 400,
                                                  decoration: BoxDecoration(
                                                      color: corFundo,
                                                      borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                                                      boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                                                        BoxShadow(
                                                            color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                                                            blurRadius: 5) //blur radius of shadow
                                                      ]
                                                  ),
                                                  child: Center(
                                                    child: DropdownButton<String>(
                                                      iconEnabledColor: Colors.white,
                                                      itemHeight: valorItemHeightSerie,
                                                      padding: EdgeInsets.all(10),
                                                      isExpanded: true,
                                                      value: dropdownValue,
                                                      icon: Transform.translate(
                                                        offset: const Offset(3, 0),
                                                        child: const Icon(Icons.arrow_drop_down),
                                                      ),
                                                      elevation: 16,
                                                      dropdownColor: corFundo,
                                                      style: const TextStyle(color: Colors.white, fontSize: 15),
                                                      focusColor: Theme.of(context).scaffoldBackgroundColor,
                                                      underline: Container(
                                                        height: 0,
                                                        color: Colors.deepPurpleAccent,
                                                      ),
                                                      onChanged: (String? value) {

                                                        // This is called when the user selects an item.
                                                        setState(() {

                                                          dropdownValue = value!;
                                                          listaMostrarMetrica = listaEscolhida.where((element) => element.nome==dropdownValue).map((e) => e.metrica.toString()).toSet().toList();
                                                          listaMostrarMetrica.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                          dropdownValueMetrica = listaMostrarMetrica.first;

                                                          listaMostrarNivelGeog = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica).map((e) => e.nivelGeografico.toString()).toSet().toList();
                                                          listaMostrarNivelGeog.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                          dropdownValueNivelGeog = listaMostrarNivelGeog.first;

                                                          listaMostrarLocalidade = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog).map((e) => e.localidades.toString()).toSet().toList();
                                                          listaMostrarLocalidade.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                          dropdownValueLocalidade = listaMostrarLocalidade.first;

                                                          listaMostrarCategoria_aux = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade).map((e) => e.categoria.toString()).toSet().toList();

                                                          // Separar o indice geral em um objeto a parte
                                                          String? specialAssunto;
                                                          List<String> otherAssuntos = [];
                                                          for (var assunto in listaMostrarCategoria_aux) {
                                                            if (removeDiacritics(assunto).toLowerCase() == 'indice geral') {
                                                              specialAssunto = assunto;
                                                            } else {
                                                              otherAssuntos.add(assunto);
                                                            }
                                                          }
                                                          // ordenar os dados
                                                          otherAssuntos.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));

                                                          listaMostrarCategoria = [];
                                                          // combinar o objeto com o indice geral e a lista ordenada
                                                          if (specialAssunto != null) {
                                                            listaMostrarCategoria.add(specialAssunto);
                                                          }
                                                          listaMostrarCategoria.addAll(otherAssuntos);

                                                          dropdownValueCategoria = listaMostrarCategoria.first;

                                                          urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;

                                                          fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
                                                          cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

                                                          nomeSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).nome;
                                                          formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;

                                                          periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                              element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                              element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;
                                                          if(periodicidade=="anual"){
                                                            formatoData = "yyyy";
                                                            formatoDataGrafico = "yyyy";
                                                          } else if(periodicidade=="diária"){
                                                            formatoData = "dd/MM/yyyy";
                                                            formatoDataGrafico = "dd/MM/yy";
                                                          } else {
                                                            formatoData = "MM/yyyy";
                                                            formatoDataGrafico = "MM/yy";
                                                          }
                                                          chartData.clear();
                                                          if(fonte=="Banco Central do Brasil"){
                                                            loadDataSGS();
                                                          } else if(fonte == "IBGE")  {
                                                            loadDataIBGE();
                                                          } else if(fonte == "IPEADATA"){
                                                            loadDataIPEADATA();
                                                          } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
                                                            loadDataRTN();
                                                          } else {
                                                            loadDataFocus();
                                                          }
                                                        });
                                                      },
                                                      items: listaMostrar.map<DropdownMenuItem<String>>((String value) {
                                                        return DropdownMenuItem<String>(
                                                          value: value,
                                                          child: Center(child: Text(value, textAlign: TextAlign.center,)),
                                                        );
                                                      }).toList(),
                                                    ),
                                                  ),
                                                ),
                                                Padding(padding: EdgeInsets.only(top: 15)),
                                                Text(
                                                  "Selecione a métrica:",
                                                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                ),
                                                Padding(padding: EdgeInsets.only(bottom: 5)),
                                                Container(
                                                    height: alturaMetrica,
                                                    width: 400,
                                                    decoration: BoxDecoration(
                                                        color: corFundo,
                                                        borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                                                        boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                                                          BoxShadow(
                                                              color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                                                              blurRadius: 5) //blur radius of shadow
                                                        ]
                                                    ),
                                                    child: Center(
                                                      child: DropdownButton<String>(
                                                        padding: EdgeInsets.all(10),
                                                        iconEnabledColor: Colors.white,
                                                        itemHeight: valorItemHeightMetrica,
                                                        isExpanded: true,
                                                        value: dropdownValueMetrica,
                                                        icon: Transform.translate(
                                                          offset: const Offset(3, 0),
                                                          child: const Icon(Icons.arrow_drop_down),
                                                        ),
                                                        elevation: 16,
                                                        dropdownColor: corFundo,
                                                        style: const TextStyle(color: Colors.white, fontSize: 15),
                                                        focusColor: Theme.of(context).scaffoldBackgroundColor,
                                                        underline: Container(
                                                          height: 0,
                                                          color: Colors.deepPurpleAccent,
                                                        ),
                                                        onChanged: (String? value) {
                                                          // This is called when the user selects an item.
                                                          setState(() {
                                                            dropdownValueMetrica = value!;
                                                            listaMostrarNivelGeog = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica).map((e) => e.nivelGeografico.toString()).toSet().toList();
                                                            listaMostrarNivelGeog.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            dropdownValueNivelGeog = listaMostrarNivelGeog.first;

                                                            listaMostrarLocalidade = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog).map((e) => e.localidades.toString()).toSet().toList();
                                                            listaMostrarLocalidade.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            dropdownValueLocalidade = listaMostrarLocalidade.first;

                                                            listaMostrarCategoria_aux = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade).map((e) => e.categoria.toString()).toSet().toList();

                                                            // Separar o indice geral em um objeto a parte
                                                            String? specialAssunto;
                                                            List<String> otherAssuntos = [];
                                                            for (var assunto in listaMostrarCategoria_aux) {
                                                              if (removeDiacritics(assunto).toLowerCase() == 'indice geral') {
                                                                specialAssunto = assunto;
                                                              } else {
                                                                otherAssuntos.add(assunto);
                                                              }
                                                            }
                                                            // ordenar os dados
                                                            otherAssuntos.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            // combinar o objeto com o indice geral e a lista ordenada
                                                            listaMostrarCategoria = [];
                                                            if (specialAssunto != null) {
                                                              listaMostrarCategoria.add(specialAssunto);
                                                            }
                                                            listaMostrarCategoria.addAll(otherAssuntos);

                                                            dropdownValueCategoria = listaMostrarCategoria.first;

                                                            urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;
                                                            fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
                                                            cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

                                                            formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;
                                                            periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;
                                                            if(periodicidade=="anual"){
                                                              formatoData = "yyyy";
                                                              formatoDataGrafico = "yyyy";
                                                            } else if(periodicidade=="diária"){
                                                              formatoData = "dd/MM/yyyy";
                                                              formatoDataGrafico = "dd/MM/yy";
                                                            } else {
                                                              formatoData = "MM/yyyy";
                                                              formatoDataGrafico = "MM/yy";
                                                            }
                                                            chartData.clear();
                                                            if(fonte=="Banco Central do Brasil"){
                                                              loadDataSGS();
                                                            } else if(fonte == "IBGE")  {
                                                              loadDataIBGE();
                                                            } else if(fonte == "IPEADATA"){
                                                              loadDataIPEADATA();
                                                            } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
                                                              loadDataRTN();
                                                            } else {
                                                              loadDataFocus();
                                                            }
                                                          });
                                                        },
                                                        items: listaMostrarMetrica.map<DropdownMenuItem<String>>((String value) {
                                                          return DropdownMenuItem<String>(
                                                            value: value,
                                                            child: Center(child: Text(value, textAlign: TextAlign.center,)),
                                                          );
                                                        }).toList(),
                                                      ),
                                                    )
                                                ),
                                                Padding(padding: EdgeInsets.only(top: 15)),
                                                Text(
                                                  "Selecione o nível geográfico:",
                                                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                ),
                                                Padding(padding: EdgeInsets.only(bottom: 5)),
                                                Container(
                                                    height: 40,
                                                    width: 400,
                                                    decoration: BoxDecoration(
                                                        color: corFundo,
                                                        borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                                                        boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                                                          BoxShadow(
                                                              color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                                                              blurRadius: 5) //blur radius of shadow
                                                        ]
                                                    ),
                                                    child: Center(
                                                      child: DropdownButton<String>(
                                                        iconEnabledColor: Colors.white,
                                                        isExpanded: true,
                                                        value: dropdownValueNivelGeog,
                                                        icon: Transform.translate(
                                        offset: const Offset(-7, 0),
                                        child: const Icon(Icons.arrow_drop_down),
                                        ),
                                                        elevation: 16,
                                                        style: const TextStyle(color: Colors.white, fontSize: 15),
                                                        focusColor: Theme.of(context).scaffoldBackgroundColor,
                                                        dropdownColor: corFundo,
                                                        underline: Container(
                                                          height: 0,
                                                          color: Colors.deepPurpleAccent,
                                                        ),
                                                        onChanged: (String? value) {
                                                          // This is called when the user selects an item.
                                                          setState(() {
                                                            dropdownValueNivelGeog = value!;
                                                            listaMostrarLocalidade = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog).map((e) => e.localidades.toString()).toSet().toList();
                                                            listaMostrarLocalidade.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            dropdownValueLocalidade = listaMostrarLocalidade.first;

                                                            listaMostrarCategoria_aux = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade).map((e) => e.categoria.toString()).toSet().toList();

                                                            // Separar o indice geral em um objeto a parte
                                                            String? specialAssunto;
                                                            List<String> otherAssuntos = [];
                                                            for (var assunto in listaMostrarCategoria_aux) {
                                                              if (removeDiacritics(assunto).toLowerCase() == 'indice geral') {
                                                                specialAssunto = assunto;
                                                              } else {
                                                                otherAssuntos.add(assunto);
                                                              }
                                                            }
                                                            // ordenar os dados
                                                            otherAssuntos.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            // combinar o objeto com o indice geral e a lista ordenada
                                                            listaMostrarCategoria = [];
                                                            if (specialAssunto != null) {
                                                              listaMostrarCategoria.add(specialAssunto);
                                                            }
                                                            listaMostrarCategoria.addAll(otherAssuntos);

                                                            dropdownValueCategoria = listaMostrarCategoria.first;

                                                            urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;
                                                            fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
                                                            cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

                                                            formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;
                                                            periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;
                                                            if(periodicidade=="anual"){
                                                              formatoData = "yyyy";
                                                              formatoDataGrafico = "yyyy";
                                                            } else if(periodicidade=="diária"){
                                                              formatoData = "dd/MM/yyyy";
                                                              formatoDataGrafico = "dd/MM/yy";
                                                            } else {
                                                              formatoData = "MM/yyyy";
                                                              formatoDataGrafico = "MM/yy";
                                                            }
                                                            chartData.clear();
                                                            if(fonte=="Banco Central do Brasil"){
                                                              loadDataSGS();
                                                            } else if(fonte == "IBGE")  {
                                                              loadDataIBGE();
                                                            } else if(fonte == "IPEADATA"){
                                                              loadDataIPEADATA();
                                                            } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
                                                              loadDataRTN();
                                                            } else {
                                                              loadDataFocus();
                                                            }
                                                          });
                                                        },
                                                        items: listaMostrarNivelGeog.map<DropdownMenuItem<String>>((String value) {
                                                          return DropdownMenuItem<String>(
                                                            value: value,
                                                            child: Center(child: Text(value, textAlign: TextAlign.center,)),
                                                          );
                                                        }).toList(),
                                                      ),
                                                    )
                                                ),
                                                Padding(padding: EdgeInsets.only(top: 10)),
                                                Text(
                                                  "Selecione a localidade:",
                                                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                ),
                                                Padding(padding: EdgeInsets.only(bottom: 5)),
                                                Container(
                                                    height: 40,
                                                    width: 400,
                                                    decoration: BoxDecoration(
                                                        color: corFundo,
                                                        borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                                                        boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                                                          BoxShadow(
                                                              color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                                                              blurRadius: 5) //blur radius of shadow
                                                        ]
                                                    ),
                                                    child: Center(
                                                      child: DropdownButton<String>(
                                                        iconEnabledColor: Colors.white,
                                                        isExpanded: true,
                                                        value: dropdownValueLocalidade,
                                                        icon: Transform.translate(
                                                          offset: const Offset(-7, 0),
                                                          child: const Icon(Icons.arrow_drop_down),
                                                        ),
                                                        elevation: 16,
                                                        style: const TextStyle(color: Colors.white, fontSize: 15),
                                                        focusColor: Theme.of(context).scaffoldBackgroundColor,
                                                        dropdownColor: corFundo,
                                                        underline: Container(
                                                          height: 0,
                                                          color: Colors.deepPurpleAccent,
                                                        ),
                                                        onChanged: (String? value) {
                                                          // This is called when the user selects an item.
                                                          setState(() {
                                                            dropdownValueLocalidade = value!;
                                                            listaMostrarCategoria_aux = listaEscolhida.where((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade).map((e) => e.categoria.toString()).toSet().toList();

                                                            // Separar o indice geral em um objeto a parte
                                                            String? specialAssunto;
                                                            List<String> otherAssuntos = [];
                                                            for (var assunto in listaMostrarCategoria_aux) {
                                                              if (removeDiacritics(assunto).toLowerCase() == 'indice geral') {
                                                                specialAssunto = assunto;
                                                              } else {
                                                                otherAssuntos.add(assunto);
                                                              }
                                                            }
                                                            // ordenar os dados
                                                            otherAssuntos.sort((a, b) => removeDiacritics(a).compareTo(removeDiacritics(b)));
                                                            // combinar o objeto com o indice geral e a lista ordenada
                                                            listaMostrarCategoria = [];
                                                            if (specialAssunto != null) {
                                                              listaMostrarCategoria.add(specialAssunto);
                                                            }
                                                            listaMostrarCategoria.addAll(otherAssuntos);

                                                            dropdownValueCategoria = listaMostrarCategoria.first;
                                                            urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;
                                                            fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
                                                            cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

                                                            formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;
                                                            periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;
                                                            if(periodicidade=="anual"){
                                                              formatoData = "yyyy";
                                                              formatoDataGrafico = "yyyy";
                                                            } else if(periodicidade=="diária"){
                                                              formatoData = "dd/MM/yyyy";
                                                              formatoDataGrafico = "dd/MM/yy";
                                                            } else {
                                                              formatoData = "MM/yyyy";
                                                              formatoDataGrafico = "MM/yy";
                                                            }
                                                            chartData.clear();
                                                            if(fonte=="Banco Central do Brasil"){
                                                              loadDataSGS();
                                                            } else if(fonte == "IBGE")  {
                                                              loadDataIBGE();
                                                            } else if(fonte == "IPEADATA"){
                                                              loadDataIPEADATA();
                                                            } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
                                                              loadDataRTN();
                                                            } else {
                                                              loadDataFocus();
                                                            }
                                                          });
                                                        },
                                                        items: listaMostrarLocalidade.map<DropdownMenuItem<String>>((String value) {
                                                          return DropdownMenuItem<String>(
                                                            value: value,
                                                            child: Center(child: Text(value, textAlign: TextAlign.center,)),
                                                          );
                                                        }).toList(),
                                                      ),
                                                    )
                                                ),
                                                Padding(padding: EdgeInsets.only(top: 10)),
                                                Text(
                                                  "Selecione o grupo:",
                                                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                ),
                                                Padding(padding: EdgeInsets.only(bottom: 5)),
                                                Container(
                                                    height: alturaCategoria,
                                                    width: 400,
                                                    decoration: BoxDecoration(
                                                        color: corFundo,
                                                        borderRadius: BorderRadius.circular(30), //border raiuds of dropdown button
                                                        boxShadow: <BoxShadow>[ //apply shadow on Dropdown button
                                                          BoxShadow(
                                                              color: Color.fromRGBO(0, 0, 0, 0.57), //shadow for button
                                                              blurRadius: 5) //blur radius of shadow
                                                        ]
                                                    ),
                                                    child: Center(
                                                        child: DropdownButtonHideUnderline(
                                                          child: DropdownButton<String>(
                                                            iconEnabledColor: Colors.white,
                                                            itemHeight: valorItemHeightCategoria,
                                                            padding: EdgeInsets.all(10),
                                                            //isDense: true,
                                                            isExpanded: true,
                                                            value: dropdownValueCategoria,
                                                            icon: Transform.translate(
                                                              offset: const Offset(3, 0),
                                                              child: const Icon(Icons.arrow_drop_down),
                                                            ),
                                                            elevation: 16,
                                                            style: const TextStyle(color: Colors.white, fontSize: 15),
                                                            focusColor: Theme.of(context).scaffoldBackgroundColor,
                                                            dropdownColor: corFundo,
                                                            underline: Container(
                                                              height: 0,
                                                              color: Colors.deepPurpleAccent,
                                                            ),
                                                            onChanged: (String? value) {
                                                              // This is called when the user selects an item.
                                                              setState(() {
                                                                dropdownValueCategoria = value!;
                                                                urlSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                    element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                    element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).urlAPI;
                                                                fonte = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                    element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                    element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).fonte;
                                                                cod_serie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                    element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                    element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).numero;

                                                                formatoSerie = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                    element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                    element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).formato;
                                                                periodicidade = listaEscolhida.firstWhere((element) => element.nome==dropdownValue &&
                                                                    element.metrica==dropdownValueMetrica && element.nivelGeografico==dropdownValueNivelGeog &&
                                                                    element.localidades==dropdownValueLocalidade && element.categoria==dropdownValueCategoria).periodicidade;
                                                                // salvar as variaveis para serem mostradas no notificacao, caso o usuario escolha receber notificacao
                                                                if(periodicidade=="anual"){
                                                                  formatoData = "yyyy";
                                                                  formatoDataGrafico = "yyyy";
                                                                } else if(periodicidade=="diária"){
                                                                  formatoData = "dd/MM/yyyy";
                                                                  formatoDataGrafico = "dd/MM/yy";
                                                                } else {
                                                                  formatoData = "MM/yyyy";
                                                                  formatoDataGrafico = "MM/yy";
                                                                }
                                                                chartData.clear();
                                                                if(fonte=="Banco Central do Brasil"){
                                                                  loadDataSGS();
                                                                } else if(fonte == "IBGE")  {
                                                                  loadDataIBGE();
                                                                } else if(fonte == "IPEADATA"){
                                                                  loadDataIPEADATA();
                                                                } else if(fonte == "Secretaria do Tesouro Nacional (STN)"){
                                                                  loadDataRTN();
                                                                } else {
                                                                  loadDataFocus();
                                                                }
                                                              });
                                                            },
                                                            items: listaMostrarCategoria.map<DropdownMenuItem<String>>((String value) {
                                                              return DropdownMenuItem<String>(
                                                                value: value,
                                                                child: Center(child: Text(value, textAlign: TextAlign.center,)),
                                                              );
                                                            }).toList(),
                                                          ),
                                                        )
                                                    )
                                                ),
                                                Padding(padding: EdgeInsets.all(10)),
                                                Text(
                                                  "Selecione o intervalo:",
                                                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                ),
                                                  notFound ? Padding(
                                                  padding: const EdgeInsets.only(top: 10.0),
                                                  child: Text(notFoundText,
                                                    style: TextStyle(fontSize: 18, color: Colors.red), textAlign: TextAlign.justify,),
                                                ) : metodoSelecaoInicial(periodicidade),

                                                //metodoSelecaoFinal(periodicidade),
                                                notFound ? Container() : Padding(
                                                  padding: EdgeInsets.only(top: 20, bottom: 10),
                                                  child: Text(
                                                    "Gráfico: $nomeSerie - $dropdownValueLocalidade - $dropdownValueCategoria - $formatoSerie",
                                                    style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                  ),
                                                ),
                                                notFound ? Container() : Column(
                                                  children: <Widget>[
                                                    Center(child:

                                                    SfCartesianChart(
                                                      margin: EdgeInsets.only(left: 5),
                                                      primaryXAxis: CategoryAxis(
                                                        axisLabelFormatter: (AxisLabelRenderDetails args) {
                                                          late String text;
                                                          text = DateFormat(formatoDataGrafico).format(DateTime.parse(args.text)).toString();
                                                          return ChartAxisLabel(text, args.textStyle);
                                                        },
                                                      ),
                                                      primaryYAxis: NumericAxis(
                                                        //Formatting the labels in locale’s currency pattern with symbol.
                                                        numberFormat: NumberFormat.decimalPattern('pt_BR'),
                                                      ),
                                                      series:
                                                      <CartesianSeries<dynamic, String>>[
                                                        LineSeries<dynamic, String>(
                                                          dataSource: filtrarDados(),
                                                          xValueMapper: (variavel, _) => variavel.data.toString(),
                                                          yValueMapper: (variavel, _) => variavel.valor, dataLabelMapper: (data, _) => f.format(data.valor),
                                                          dataLabelSettings: DataLabelSettings(
                                                            isVisible: true, textStyle:
                                                          TextStyle(fontSize: 11),
                                                          ),
                                                          markerSettings: MarkerSettings(isVisible: true), ),
                                                      ],
                                                    ),
                                                    ),
                                                    Container(height: 160,
                                                      alignment: AlignmentDirectional.bottomCenter,
                                                      child: Column(
                                                        mainAxisAlignment: MainAxisAlignment.center,
                                                        crossAxisAlignment: CrossAxisAlignment.center,
                                                        children: <Widget>[
                                                          FutureBuilder(
                                                              future: getJsonFromRestAPI(urlSerie),
                                                              builder: (ctx, snapshot) {
                                                                if (snapshot.connectionState == ConnectionState.done) {
                                                                  return Text(
                                                                    '',
                                                                    style: TextStyle(fontSize: 0),
                                                                  );
                                                                } else
                                                                  return CircularProgressIndicator();
                                                              }
                                                          ),
                                                          Padding(
                                                            padding: EdgeInsets.only(bottom: 15),
                                                            child: Text(
                                                              "Fonte: $fonte",
                                                              style: TextStyle(fontSize: 10),
                                                            ),
                                                          ),
                                                          OutlinedButton(
                                                              onPressed: (){
                                                                Navigator.push(
                                                                    context,
                                                                    MaterialPageRoute(builder: (context) => DescricaoSeries(cod_series: cod_serie,))
                                                                );
                                                              },
                                                              style: ButtonStyle(foregroundColor: WidgetStateProperty.all(Colors.white),
                                                              backgroundColor: WidgetStateProperty.all(Color(0xFF42A5F5))),
                                                              child: Text("Descrição da série")
                                                          ),
                                                        ],
                                                      ),
                                                    ),
                                                    Padding(
                                                      padding: EdgeInsets.only(top: 5, bottom: 0),
                                                      child: Text(
                                                        "Dados do gráfico",
                                                        style: TextStyle(fontSize: 16, fontWeight: FontWeight.bold),
                                                      ),
                                                    ),
                                                    DataTable(
                                                      columnSpacing: 50,
                                                      columns: const <DataColumn>[
                                                        DataColumn(
                                                          label: Text(
                                                            'Data',
                                                            style: TextStyle(
                                                              fontStyle: FontStyle.italic,
                                                            ),
                                                          ),
                                                        ),
                                                        DataColumn(
                                                          label: Text(
                                                            'Valor',
                                                            style: TextStyle(
                                                              fontStyle: FontStyle.italic,
                                                            ),
                                                          ),
                                                        ),
                                                      ],
                                                      rows: filtrarDados()
                                                          .map(
                                                            (e) => DataRow(
                                                          cells: [
                                                            DataCell(
                                                              Text(
                                                                DateFormat(formatoData).format(e.data).toString(),
                                                                style: TextStyle(
                                                                  fontStyle: FontStyle.italic,
                                                                ),
                                                              ),
                                                            ),
                                                            DataCell(
                                                              Text(
                                                                //e.valor.toStringAsFixed(2).replaceAll('.', ','),
                                                                f.format(e.valor),
                                                                style: TextStyle(
                                                                  fontStyle: FontStyle.italic,
                                                                ),
                                                              ),
                                                            ),
                                                          ],
                                                        ),
                                                      )
                                                          .toList(),
                                                    ),
                                                  ],
                                                ),
                                                notFound ? Container() : Padding(
                                                  padding: EdgeInsets.only(bottom: 15),
                                                  child: Text(
                                                    "Fonte: $fonte",
                                                    style: TextStyle(fontSize: 10),
                                                  ),
                                                ),
                                                Row(
                                                  mainAxisAlignment: MainAxisAlignment.center,
                                                  children: <Widget>[
                                                    notFound ? Container() : OutlinedButton(
                                                        onPressed: (){
                                                          Navigator.push(
                                                              context,
                                                              MaterialPageRoute(builder: (context) => ReportarErro(codigoSerie: cod_serie,))
                                                          );
                                                        },
                                                        style: ButtonStyle(
                                                            foregroundColor: WidgetStateProperty.all(Colors.white),
                                                            backgroundColor: WidgetStateProperty.all(Color(0xFFBE3144))),
                                                        child: Text("Reportar erro")
                                                    ),
                                                    Padding(padding: EdgeInsets.only(left: 10, right: 10)),
                                                    notFound ? Container() : OutlinedButton(
                                                        onPressed: () async {
                                                          await createAndShareCSV();
                                                        },
                                                        style: ButtonStyle(
                                                            foregroundColor: WidgetStateProperty.all(Colors.white),
                                                            backgroundColor: WidgetStateProperty.all(Colors.grey[600])),
                                                        child: Text("Baixar a série")
                                                    ),
                                                  ],
                                                ),
                                                Padding(padding: EdgeInsets.only(bottom: 10)),
                                              ],
                                            );
                                        }
                                      }
                                      ),
                                  ),
                          )
                        ],
                      ),
                    ],
                  ),
                ),
              ],
            )
        )
    );
  }
}
