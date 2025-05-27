unit CamType;

interface

type
  TCamType = (
    /// <summary>
    /// Not Set
    /// </summary>
    ctNone,

    /// <summary>
    /// Vertical Part (Positive Radius, Negative Radius, Flatten)
    /// </summary>
    ctVPA,

    /// <summary>
    /// Vertical Part Cone
    /// </summary>
    ctVPC,

    /// <summary>
    /// Side of Vertical Part
    /// </summary>
    ctVPS,

    /// <summary>
    /// Horizontal Part without Radius
    /// </summary>
    ctHPNR,

    /// <summary>
    /// Horizontal Part (Full Radius for Negative or Positive)
    /// </summary>
    ctHPFR,

    /// <summary>
    /// Horizontal Part (Half Radius for Negative or Positive)
    /// </summary>
    ctHPHR);

implementation

end.
