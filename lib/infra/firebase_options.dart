import 'package:firebase_core/firebase_core.dart' show FirebaseOptions;
import 'package:flutter/foundation.dart' show defaultTargetPlatform, kIsWeb, TargetPlatform;
import 'package:flutter_dotenv/flutter_dotenv.dart';

/*class DefaultFirebaseOptions {
  static FirebaseOptions get currentPlatform {
    if (kIsWeb) {
      return FirebaseOptions(
        apiKey: $FIREBASE_WEB_API_KEY,
        appId: $FIREBASE_WEB_APPID,
        messagingSenderId: $FIREBASE_MESSAGINGSENDERID,
        projectId: $FIREBASE_PROJECTID,
        authDomain: $FIREBASE_AUTHDOMAIN,
        storageBucket: $FIREBASE_STORAGEBUCKET,
        measurementId: $FIREBASE_WEB_MEASUREMENTID,
      );
    }
    switch (defaultTargetPlatform) {
      case TargetPlatform.android:
        return FirebaseOptions(
          apiKey: $FIREBASE_ANDROID_API_KEY,
          appId: $FIREBASE_ANDROID_APPID,
          messagingSenderId: $FIREBASE_MESSAGINGSENDERID,
          projectId: $FIREBASE_PROJECTID,
          storageBucket: $FIREBASE_STORAGEBUCKET,
        );
      case TargetPlatform.iOS:
        return  FirebaseOptions(
          apiKey: $FIREBASE_IOS_MACOS_API_KEY,
          appId: $FIREBASE_IOS_MACOS_APPID,
          messagingSenderId: $FIREBASE_MESSAGINGSENDERID,
          projectId: $FIREBASE_PROJECTID,
          storageBucket: $FIREBASE_STORAGEBUCKET,
          iosBundleId: $FIREBASE_IOS_MACOS_IOSBUNDLEID
        );
      case TargetPlatform.macOS:
        return FirebaseOptions(
          apiKey: $FIREBASE_IOS_MACOS_API_KEY,
          appId: $FIREBASE_IOS_MACOS_APPID,
          messagingSenderId: $FIREBASE_MESSAGINGSENDERID,
          projectId: $FIREBASE_PROJECTID,
          storageBucket: $FIREBASE_STORAGEBUCKET,
          iosBundleId: $FIREBASE_IOS_MACOS_IOSBUNDLEID
        );
      case TargetPlatform.windows:
        return FirebaseOptions(
          apiKey: $FIREBASE_WINDOWS_API_KEY,
          appId: $FIREBASE_WINDOWS_APPID,
          messagingSenderId: $FIREBASE_MESSAGINGSENDERID,
          projectId: $FIREBASE_PROJECTID,
          authDomain: $FIREBASE_AUTHDOMAIN,
          storageBucket: $FIREBASE_STORAGEBUCKET,
          measurementId: $FIREBASE_WINDOWS_MEASUREMENTID,
        );
      case TargetPlatform.linux:
        throw UnsupportedError(
          'DefaultFirebaseOptions have not been configured for linux - '
          'you can reconfigure this by running the FlutterFire CLI again.',
        );
      default:
        throw UnsupportedError(
          'DefaultFirebaseOptions are not supported for this platform.',
        );
    }
  }
}*/



class DefaultFirebaseOptions {
  static FirebaseOptions get currentPlatform {
    if (kIsWeb) {
      return FirebaseOptions(
        apiKey: dotenv.get('FIREBASE_WEB_API_KEY'),
        appId: dotenv.get('FIREBASE_WEB_APPID'),
        messagingSenderId: dotenv.get('FIREBASE_MESSAGINGSENDERID'),
        projectId: dotenv.get('FIREBASE_PROJECTID'),
        authDomain: dotenv.get('FIREBASE_AUTHDOMAIN'),
        storageBucket: dotenv.get('FIREBASE_STORAGEBUCKET'),
        measurementId: dotenv.get('FIREBASE_WEB_MEASUREMENTID'),
      );
    }
    switch (defaultTargetPlatform) {
      case TargetPlatform.android:
        return FirebaseOptions(
          apiKey: dotenv.get('FIREBASE_ANDROID_API_KEY'),
          appId: dotenv.get('FIREBASE_ANDROID_APPID'),
          messagingSenderId: dotenv.get('FIREBASE_MESSAGINGSENDERID'),
          projectId: dotenv.get('FIREBASE_PROJECTID'),
          storageBucket: dotenv.get('FIREBASE_STORAGEBUCKET'),
        );
      case TargetPlatform.iOS:
        return  FirebaseOptions(
            apiKey: dotenv.get('FIREBASE_IOS_MACOS_API_KEY'),
            appId: dotenv.get('FIREBASE_IOS_MACOS_APPID'),
            messagingSenderId: dotenv.get('FIREBASE_MESSAGINGSENDERID'),
            projectId: dotenv.get('FIREBASE_PROJECTID'),
            storageBucket: dotenv.get('FIREBASE_STORAGEBUCKET'),
            iosBundleId: dotenv.get('FIREBASE_IOS_MACOS_IOSBUNDLEID')
        );
      case TargetPlatform.macOS:
        return FirebaseOptions(
            apiKey: dotenv.get('FIREBASE_IOS_MACOS_API_KEY'),
            appId: dotenv.get('FIREBASE_IOS_MACOS_APPID'),
            messagingSenderId: dotenv.get('FIREBASE_MESSAGINGSENDERID'),
            projectId: dotenv.get('FIREBASE_PROJECTID'),
            storageBucket: dotenv.get('FIREBASE_STORAGEBUCKET'),
            iosBundleId: dotenv.get('FIREBASE_IOS_MACOS_IOSBUNDLEID')
        );
      case TargetPlatform.windows:
        return FirebaseOptions(
          apiKey: dotenv.get('FIREBASE_WINDOWS_API_KEY'),
          appId: dotenv.get('FIREBASE_WINDOWS_APPID'),
          messagingSenderId: dotenv.get('FIREBASE_MESSAGINGSENDERID'),
          projectId: dotenv.get('FIREBASE_PROJECTID'),
          authDomain: dotenv.get('FIREBASE_AUTHDOMAIN'),
          storageBucket: dotenv.get('FIREBASE_STORAGEBUCKET'),
          measurementId: dotenv.get('FIREBASE_WINDOWS_MEASUREMENTID'),
        );
      case TargetPlatform.linux:
        throw UnsupportedError(
          'DefaultFirebaseOptions have not been configured for linux - '
              'you can reconfigure this by running the FlutterFire CLI again.',
        );
      default:
        throw UnsupportedError(
          'DefaultFirebaseOptions are not supported for this platform.',
        );
    }
  }
}
