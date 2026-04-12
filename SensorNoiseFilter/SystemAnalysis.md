# Sensör Verisi Gürültü Temizleme - Sistem Analizi

## 1. Proje Tanımı

Bu proje, titreşim sensöründen alınan verilerdeki gürültüyü temizlemek amacıyla geliştirilmiştir.
CSV formatında okunan sensör verileri (x, y, z eksenleri) üzerinde çeşitli dijital filtreler uygulanarak
gürültü azaltılır ve sonuçlar grafiksel olarak görselleştirilir.

## 2. Gürültü Temizleme Yöntemleri Araştırması

### 2.1 Matematiksel ve Algoritmik Teknikler

| Teknik | Açıklama | Kullanım Alanı |
|--------|----------|----------------|
| **Hareketli Ortalama (Moving Average)** | Son N örneğin aritmetik ortalaması alınır. Basit ve etkili bir düzeltme yöntemidir. | Yavaş değişen sinyallerde gürültü azaltma |
| **Üstel Düzeltme (Exponential Smoothing)** | Ağırlıklı ortalama ile eski ve yeni değerler arasında yumuşak geçiş sağlar. Alpha parametresi ile ayarlanır. | Gerçek zamanlı veri düzeltme |
| **Medyan Filtre** | Pencere içindeki değerlerin medyanını alır. Ani sıçramalara (spike) karşı dayanıklıdır. | Darbe gürültüsü temizleme |
| **Kalman Filtresi** | Durum tahmini yapan özyinelemeli bir filtredir. Ölçüm ve süreç gürültüsünü modelleyerek optimal tahmin üretir. | Dinamik sistem tahmini |
| **Düşük Geçişli Filtre (Low-Pass)** | Yüksek frekanslı bileşenleri bastırır, düşük frekanslı sinyali geçirir. | Yüksek frekanslı gürültü temizleme |
| **Yüksek Geçişli Filtre (High-Pass)** | Düşük frekanslı bileşenleri bastırır, yüksek frekanslı sinyali geçirir. | DC offset ve drift temizleme |
| **Bant Geçişli Filtre (Band-Pass)** | Belirli bir frekans aralığındaki sinyalleri geçirir. | Belirli frekans bandındaki sinyal izolasyonu |

### 2.2 Filtreleme Yöntemleri Detayları

#### Düşük Geçişli Filtre (Low-Pass Filter)
- **Formül:** y[n] = α × x[n] + (1 - α) × y[n-1]
- **α (alpha):** Kesim frekansı parametresi (0 < α ≤ 1)
- α küçüldükçe daha fazla yumuşatma yapılır
- α = dt / (RC + dt) formülü ile hesaplanır

#### Yüksek Geçişli Filtre (High-Pass Filter)
- **Formül:** y[n] = α × (y[n-1] + x[n] - x[n-1])
- **α (alpha):** Kesim frekansı parametresi (0 < α ≤ 1)
- α büyüdükçe daha düşük frekanslı sinyaller geçer

#### Bant Geçişli Filtre (Band-Pass Filter)
- Düşük geçişli ve yüksek geçişli filtrelerin seri bağlanmasıyla oluşturulur
- **Alt kesim frekansı** ve **üst kesim frekansı** parametreleri ile kontrol edilir

#### Hareketli Ortalama (Moving Average)
- **Formül:** y[n] = (1/N) × Σ x[n-k] (k=0..N-1)
- **N:** Pencere boyutu (Window Size)
- N arttıkça daha fazla düzeltme, ancak daha fazla gecikme

#### Medyan Filtre
- Pencere içindeki değerler sıralanır, ortadaki değer seçilir
- **N:** Pencere boyutu (tek sayı olmalı)
- Ani sıçramalara karşı hareketli ortalamadan daha dayanıklıdır

#### Kalman Filtresi
- **Q:** Süreç gürültüsü kovaryansı (Process Noise)
- **R:** Ölçüm gürültüsü kovaryansı (Measurement Noise)
- **P:** Tahmin hatası kovaryansı
- Q/R oranı filtrenin davranışını belirler

### 2.3 Sinyal İşleme Analiz Yöntemleri

1. **İstatistiksel Analiz:** Ortalama, standart sapma, varyans hesaplama ile gürültü seviyesi belirleme
2. **Frekans Analizi:** FFT (Hızlı Fourier Dönüşümü) ile frekans bileşenlerini analiz etme
3. **Zaman Serisi Analizi:** Trend, mevsimsellik ve gürültü bileşenlerini ayırma
4. **Gürültü Tespiti:** Standart sapma tabanlı eşikleme ile gürültü noktalarını tespit etme

## 3. Sistem Mimarisi (MVC)

```
┌─────────────────────────────────────────────────┐
│                    VIEW                          │
│  MainForm.pas                                    │
│  - TPaintBox (Grafik çizimi)                     │
│  - TComboBox (Filtre seçimi)                     │
│  - TTrackBar / TEdit (Parametre ayarları)        │
│  - TButton (İşlem butonları)                     │
│  - TOpenDialog (CSV dosya seçimi)                │
└────────────────────┬────────────────────────────┘
                     │
┌────────────────────┴────────────────────────────┐
│                 CONTROLLER                       │
│  FilterController.pas                            │
│  - CSV yükleme koordinasyonu                     │
│  - Filtre uygulama koordinasyonu                 │
│  - Gürültü tespit koordinasyonu                  │
│  - View güncelleme                               │
└────────────────────┬────────────────────────────┘
                     │
┌────────────────────┴────────────────────────────┐
│                   MODEL                          │
│  SensorDataModel.pas  │  SignalFilters.pas       │
│  - TSensorRecord      │  - TMovingAverageFilter  │
│  - TSensorDataList    │  - TMedianFilter         │
│  - CSV okuma/yazma    │  - TLowPassFilter        │
│  - İstatistik hesap   │  - THighPassFilter       │
│                       │  - TBandPassFilter       │
│                       │  - TKalmanFilter         │
│                       │  - TExponentialSmoothing  │
└───────────────────────┴─────────────────────────┘
```

## 4. Filtre Parametreleri

| Filtre | Parametre | Varsayılan | Aralık |
|--------|-----------|------------|--------|
| Hareketli Ortalama | Pencere Boyutu (N) | 5 | 2-50 |
| Medyan Filtre | Pencere Boyutu (N) | 5 | 3-51 (tek) |
| Düşük Geçişli | Alpha (α) | 0.1 | 0.01-1.0 |
| Yüksek Geçişli | Alpha (α) | 0.1 | 0.01-1.0 |
| Bant Geçişli | Alt Alpha, Üst Alpha | 0.05, 0.3 | 0.01-1.0 |
| Kalman | Q (Süreç), R (Ölçüm) | 0.01, 0.1 | 0.001-10.0 |
| Üstel Düzeltme | Alpha (α) | 0.3 | 0.01-1.0 |
| Gürültü Tespiti | Sigma Çarpanı | 2.0 | 0.5-5.0 |

## 5. Görselleştirme

- **Ham Veri:** Mavi renk ile çizilir
- **Filtrelenmiş Veri:** Yeşil renk ile çizilir
- **Gürültü Noktaları:** Kırmızı renk ile işaretlenir
- X, Y, Z eksenleri ayrı ayrı veya birlikte görüntülenebilir
- Grafik üzerinde yakınlaştırma/kaydırma desteği

## 6. CSV Dosya Formatı

```csv
"serialNumber","x","y","z","time","deviceTime"
"1010","-51.94954","-38.15263","944.36887","2026-04-10 13:51:33.918","2026-04-10 12:51:33.918323"
```

- **serialNumber:** Sensör seri numarası
- **x, y, z:** 3 eksenli ivmeölçer/titreşim verileri
- **time:** Sunucu zaman damgası
- **deviceTime:** Cihaz zaman damgası
