import 'dart:math';
import 'package:intl/intl.dart';
import 'package:cloud_firestore/cloud_firestore.dart';
import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:timezone/data/latest.dart' as tz;
import 'package:timezone/standalone.dart' as tz;

import 'TelaDados.dart';

class ReportarErro extends StatefulWidget {
  const ReportarErro({super.key, required this.codigoSerie});
  final String codigoSerie;
  @override
  State<ReportarErro> createState() => _ReportarErroState();
}

class _ReportarErroState extends State<ReportarErro> {
  var corFundo = Color.fromARGB(255, 63, 81, 181);
  late TextEditingController _controller1 = TextEditingController();
  late TextEditingController _controller2 = TextEditingController();
  var db = FirebaseFirestore.instance;
  var erroEmail = '';
  var erroTexto = '';
  String randomString = '';
  String verificationText = '';
  bool isVerified = false;
  bool showVerifiedIcon = false;
  TextEditingController controler3 = TextEditingController();

  @override
  void initState(){
    super.initState();
    _controller1 = TextEditingController();
    _controller2 = TextEditingController();
    buildCaptcha();
  }

  void buildCaptcha(){
    const letters =
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
    const length = 6;
    final random = Random();
    randomString = String.fromCharCodes(List.generate(length, (index) => letters.codeUnitAt(random.nextInt((letters.length)))));
    setState(() {});
  }

  bool isValidEmail(String email) {
    final RegExp emailRegex = RegExp(r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
    return emailRegex.hasMatch(email);
  }

  @override
  Widget build(BuildContext context) {

    return Scaffold(
      appBar: AppBar(
        title: Text("Informe os erros", style: TextStyle(color: Colors.white) ),
        backgroundColor: corFundo,
        iconTheme: IconThemeData(
          color: Colors.white, //change your color here
        ),
      ),
      body: SingleChildScrollView(
        child: Container(
          padding: EdgeInsets.only(top: 10, left: 5, right: 5, bottom: 10),
          child: Column(
            children: <Widget>[
              TextField(
                controller: _controller1,
                decoration: InputDecoration(
                  border: OutlineInputBorder(),
                  hintText: 'Informe e-mail para receber a resposta',
                  hintStyle: TextStyle(fontSize: 15)
                ),
                onSubmitted: (String value) async {
                  setState(() {
                    _controller1 = value as TextEditingController;
                  });
                },
              ),
              Padding(padding: EdgeInsets.only(top: 10)),
              TextField(
                controller: _controller2,
                maxLines: 12,
                minLines: 6,
                decoration: InputDecoration(
                    border: OutlineInputBorder(),
                    hintText: 'Informe o erro nos valores e/ou na descrição da série',
                    hintStyle: TextStyle(fontSize: 15)
                ),
                onSubmitted: (String value) async {
                  setState(() {
                    _controller2 = value as TextEditingController;
                  });
                },
              ),
              Padding(padding: EdgeInsets.all(10)),
              Container(
                padding: const EdgeInsets.all(12),
                decoration: BoxDecoration(
                  border: Border.all(width: 2),
                  borderRadius: BorderRadius.circular(8)
                ),
                child: Text(randomString,
                  style: const TextStyle(fontWeight: FontWeight.w500),
                ),
              ),
              Padding(padding: EdgeInsets.only(bottom: 5)),
              IconButton(
                  onPressed: (){
                    buildCaptcha();
                  }, 
                  icon: const Icon(Icons.refresh)
              ),
              Container(
                margin: const EdgeInsets.all(16),
                child: TextFormField(
                  onChanged: (value) {
                    setState(() {
                      isVerified = false;
                    });
                  },
                  decoration: InputDecoration(
                    border: OutlineInputBorder(),
                    hintText: "Informe o Captcha",
                    labelText: "Informe o Captcha"
                ),
                  controller: controler3,
                )
              ),
              Padding(padding: EdgeInsets.only(top: 5, bottom: 5)),
              ElevatedButton(
                  onPressed: (){
                    isVerified = controler3.text == randomString;
                    if(isVerified){
                      verificationText = "Verificado!";
                      showVerifiedIcon = true;
                    } else {
                      verificationText = "Informe o texto correto!";
                      showVerifiedIcon = false;
                      controler3 = TextEditingController();
                      buildCaptcha();
                    }
                    setState(() {});
                  },
                  child: Text("Verificar")
              ),
              Padding(padding: EdgeInsets.only(top: 5, bottom: 5)),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Visibility(
                    visible: showVerifiedIcon,
                      child: const Icon(Icons.verified)
                  ),
                  Text(verificationText)
                ],
              ),
              Padding(padding: EdgeInsets.only(top: 5, bottom: 5)),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  ElevatedButton(
                      style: ButtonStyle(
                        fixedSize: WidgetStateProperty.all<Size>(Size(120,50)),
                        backgroundColor: WidgetStateProperty.all(Color(0xFF77B254)),
                        foregroundColor: WidgetStateProperty.all(Colors.white)
                      ),
                      onPressed: isVerified ? () async {
                        if(_controller1.text.isEmpty || !isValidEmail(_controller1.text)){
                          showDialog(
                            context: context,
                            builder: (BuildContext context) {
                              return AlertDialog(
                                title: Text('Erro'),
                                content: Text('Informe um e-mail válido!'),
                                actions: <Widget>[
                                  TextButton(
                                    child: Text('OK'),
                                    onPressed: () {
                                      Navigator.of(context).pop();
                                    },
                                  ),
                                ],
                              );
                            },
                          );
                          setState(() {
                            verificationText = '';
                            showVerifiedIcon = false;
                            controler3 = TextEditingController();
                            buildCaptcha();
                          });
                          return; // This will prevent the code from proceeding
                        }
                        if(_controller2.text.isEmpty){
                          showDialog(
                            context: context,
                            builder: (BuildContext context) {
                              return AlertDialog(
                                title: Text('Erro'),
                                content: Text('Informe o erro identificado.'),
                                actions: <Widget>[
                                  TextButton(
                                    child: Text('OK'),
                                    onPressed: () {
                                      Navigator.of(context).pop();
                                    },
                                  ),
                                ],
                              );
                            },
                          );
                          setState(() {
                            verificationText = '';
                            showVerifiedIcon = false;
                            controler3 = TextEditingController();
                            buildCaptcha();
                          });
                          return; // This will prevent the code from proceeding
                        }
                        if(_controller2.text.length < 5){
                          showDialog(
                            context: context,
                            builder: (BuildContext context) {
                              return AlertDialog(
                                title: Text('Erro'),
                                content: Text('O erro deve ter pelo menos 5 caracteres.'),
                                actions: <Widget>[
                                  TextButton(
                                    child: Text('OK'),
                                    onPressed: () {
                                      Navigator.of(context).pop();
                                    },
                                  ),
                                ],
                              );
                            },
                          );
                          setState(() {
                            verificationText = '';
                            verificationText = '';
                            showVerifiedIcon = false;
                            controler3 = TextEditingController();
                            buildCaptcha();
                          });
                          return; // This will prevent the code from proceeding
                        }
                        DateTime now = DateTime.now();
                        tz.initializeTimeZones();
                        tz.Location spLocation = tz.getLocation('America/Sao_Paulo');
                        tz.TZDateTime spTime = tz.TZDateTime.from(now, spLocation);
                        String formattedDate = DateFormat('yyyy-MM-dd - kk:mm').format(spTime);
                        var inserirErro = <String, dynamic>{'codSerie': widget.codigoSerie ,'email': _controller1.text, 'erro': _controller2.text, 'data': formattedDate};
                        db.collection("erros").add(inserirErro).then((DocumentReference doc) =>
                            print('DocumentSnapshot added with ID: ${doc.id}'));
                        showDialog(
                            context: context,
                            builder: (BuildContext context){
                              return AlertDialog(
                                title: Text('Sucesso'),
                                content: Text("Erro enviado com sucesso!"),
                                actions: <Widget>[
                                  TextButton(
                                      onPressed: (){
                                        Navigator.pop(context);
                                      },
                                      child: Text('OK')
                                  )
                                ],
                              );
                            }
                        );
                        setState(() {
                          verificationText = '';
                          _controller1 = TextEditingController();
                          _controller2 = TextEditingController();
                          controler3 = TextEditingController();
                          buildCaptcha();
                        });
                      } : null,
                      child: Text("Enviar")
                  ),
                  Padding(padding: EdgeInsets.all(10)),
                  ElevatedButton(
                      style: ButtonStyle(
                          fixedSize: WidgetStateProperty.all<Size>(Size(120,50)),
                          backgroundColor: WidgetStateProperty.all(Color(0xFFBE3144)),
                          foregroundColor: WidgetStateProperty.all(Colors.white)
                      ),
                      onPressed: () async {
                        Navigator.pop(context);
                      },
                      child: Text("Cancelar")
                  ),
                  Text(erroEmail),
                ],
              )
            ],
          ),
        ),
      ),
    );
  }
}