#ifndef EAMXX_VERTICAL_REMAPPER_HPP
#define EAMXX_VERTICAL_REMAPPER_HPP

#include "share/grid/remap/abstract_remapper.hpp"

#include <ekat/util/ekat_lin_interp.hpp>

namespace scream
{

/*
 * A remapper to interpolate fields on a separate vertical grid
 */

class VerticalRemapper : public AbstractRemapper
{
public:
  enum ExtrapType {
    Mask,  // Use fixed value
    P0     // Constant extrapolation
  };

  enum TopBot {
    Top = 1,
    Bot = 2,
    TopAndBot = Top | Bot
  };

  // When setting src/tgt pressure profiles, we may not care to distinguish
  // between midpoints/interface. E.g., in output, we may remap both midpoints
  // and interface quantities to the SAME set of pressure levels.
  enum ProfileType {
    Midpoints,
    Interfaces,
    Both
  };

  // Use fixed value as mask value
  VerticalRemapper (const grid_ptr_type& src_grid,
                    const std::string& map_file);

  VerticalRemapper (const grid_ptr_type& src_grid,
                    const grid_ptr_type& tgt_grid);

  ~VerticalRemapper () = default;

  FieldLayout create_src_layout (const FieldLayout& tgt_layout) const override;
  FieldLayout create_tgt_layout (const FieldLayout& src_layout) const override;

  bool compatible_layouts (const layout_type& src,
                           const layout_type& tgt) const override {
    // Strip the LEV/ILEV tags, and check if they are the same
    // Also, check rank compatibility, in case one has LEV/ILEV and the other doesn't
    // NOTE: tgt layouts always use LEV (not ILEV), while src can have ILEV or LEV.

    using namespace ShortFieldTagsNames;
    auto src_stripped = src.clone().strip_dims({LEV,ILEV});
    auto tgt_stripped = tgt.clone().strip_dims({LEV,ILEV});

    return src.rank()==tgt.rank() and
           src_stripped.congruent(tgt_stripped);
  }

  bool is_valid_tgt_layout (const layout_type& layout) const override {
    using namespace ShortFieldTagsNames;
    return !(m_tgt_mid_same_as_int and layout.has_tag(ILEV))
           and AbstractRemapper::is_valid_tgt_layout(layout);
  }

  bool is_valid_src_layout (const layout_type& layout) const override {
    using namespace ShortFieldTagsNames;
    return !(m_src_mid_same_as_int and layout.has_tag(ILEV))
           and AbstractRemapper::is_valid_src_layout(layout);
  }

  void set_extrapolation_type (const ExtrapType etype, const TopBot where = TopAndBot);
  void set_mask_value (const Real mask_val);

  void set_source_pressure (const Field& p, const ProfileType ptype);
  void set_target_pressure (const Field& p, const ProfileType ptype);

  void set_source_pressure (const Field& pmid, const Field& pint) {
    set_source_pressure (pmid, Midpoints);
    set_source_pressure (pint, Interfaces);
  }
  void set_target_pressure (const Field& pmid, const Field& pint) {
    set_target_pressure (pmid, Midpoints);
    set_target_pressure (pint, Interfaces);
  }

  Field get_source_pressure (bool midpoints) const {
    return midpoints ? m_src_pmid : m_src_pint;
  }
  Field get_target_pressure (bool midpoints) const {
    return midpoints ? m_tgt_pmid : m_tgt_pint;
  }

  // This method simply creates the tgt grid from a map file
  static std::shared_ptr<AbstractGrid>
  create_tgt_grid (const grid_ptr_type& src_grid,
                   const std::string& map_file);

protected:

  void set_pressure (const Field& p, const std::string& src_or_tgt, const ProfileType ptype);
  FieldLayout create_layout (const FieldLayout& from_layout,
                             const std::shared_ptr<const AbstractGrid>& to_grid,
                             const bool int_same_as_mid) const;

  const identifier_type& do_get_src_field_id (const int ifield) const override {
    return m_src_fields[ifield].get_header().get_identifier();
  }
  const identifier_type& do_get_tgt_field_id (const int ifield) const override {
    return m_tgt_fields[ifield].get_header().get_identifier();
  }
  const field_type& do_get_src_field (const int ifield) const override {
    return m_src_fields[ifield];
  }
  const field_type& do_get_tgt_field (const int ifield) const override {
    return m_tgt_fields[ifield];
  }

  void do_registration_begins () override { /* Nothing to do here */ }

  void do_register_field (const identifier_type& src, const identifier_type& tgt) override;

  void do_bind_field (const int ifield, const field_type& src, const field_type& tgt) override;

  void do_registration_ends () override;

  void do_remap_fwd () override;

  void do_remap_bwd () override {
    EKAT_ERROR_MSG ("VerticalRemapper only supports fwd remapping.\n");
  }

#ifdef KOKKOS_ENABLE_CUDA
public:
#endif
  template<int N>
  void apply_vertical_interpolation (const ekat::LinInterp<Real,N>& lin_interp,
                                     const Field& f_src, const Field& f_tgt,
                                     const Field& p_src, const Field& p_tgt) const;

  void extrapolate (const Field& f_src, const Field& f_tgt,
                    const Field& p_src, const Field& p_tgt,
                    const Real mask_val) const;

  template<int N>
  void setup_lin_interp (const ekat::LinInterp<Real,N>& lin_interp,
                         const Field& p_src, const Field& p_tgt) const;
protected:

  void create_lin_interp ();
  
  using KT = KokkosTypes<DefaultDevice>;

  template<typename T>
  using view_1d = typename KT::template view_1d<T>;
  template<typename T>
  using view_2d = typename KT::template view_2d<T>;

  ekat::Comm            m_comm;

  // Source and target fields
  std::vector<Field>    m_src_fields;
  std::vector<Field>    m_tgt_fields;
  std::vector<Field>    m_src_masks;
  std::vector<Field>    m_tgt_masks;

  // Vertical profile fields, both for source and target
  Field                 m_src_pmid;
  Field                 m_src_pint;
  Field                 m_tgt_pmid;
  Field                 m_tgt_pint;

  bool m_src_mid_same_as_int = false;
  bool m_tgt_mid_same_as_int = false;

  // Extrapolation settings at top/bottom. Default to P0 extrapolation
  ExtrapType            m_etype_top = P0;
  ExtrapType            m_etype_bot = P0;
  Real                  m_mask_val = std::numeric_limits<Real>::quiet_NaN();

  // We need to remap mid/int fields separately, and we want to use packs if possible,
  // so we need to divide input fields into 4 separate categories

  // Map field id to whether it's packed/scalar and midpoint/interface
  struct FType {
    bool packed = false;
    bool midpoints = true;
  };
  std::map<std::string,FType> m_field2type;

  std::shared_ptr<ekat::LinInterp<Real,SCREAM_PACK_SIZE>> m_lin_interp_mid_packed;
  std::shared_ptr<ekat::LinInterp<Real,SCREAM_PACK_SIZE>> m_lin_interp_int_packed;
  std::shared_ptr<ekat::LinInterp<Real,1>>                m_lin_interp_mid_scalar;
  std::shared_ptr<ekat::LinInterp<Real,1>>                m_lin_interp_int_scalar;
};

} // namespace scream

#endif // EAMXX_VERTICAL_REMAPPER_HPP
